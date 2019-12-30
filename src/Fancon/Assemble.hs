module Fancon.Assemble (assemble) where

import Prelude hiding (lines)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Polysemy
import Polysemy.State
import Control.Monad (mapM_)
import Data.Maybe (isJust, isNothing, fromJust)

import qualified Fancon.Instruction as Ins
import qualified Fancon.Parse as P

data Warning = UnknownCommand Int
             | UnreferencedSymbol Int
  deriving (Show, Eq)

data Error = InvalidNumberOfArguments Int
           | DuplicateDefinition Int
           | Undefined Int
  deriving (Show, Eq)

data Symbol = Symbol { references :: [Int]
                     , exported :: Bool
                     , imported :: Bool
                     , definedAt :: Maybe Int
                     }
            deriving (Eq, Show)

emptySymbol :: Symbol
emptySymbol = Symbol { references = [], exported = False, imported = False, definedAt = Nothing }

type Symtab = M.Map Text Symbol

data AssemblerState = AssemblerState { errors :: [Error]
                                     , warnings :: [Warning]
                                     , instructions :: [Ins.Instruction]
                                     , symbolTable :: Symtab
                                     , offset :: Int
                                     , lineNumber :: Int
                                     }

initialAssemblerState :: AssemblerState
initialAssemblerState = AssemblerState { errors = []
                                       , warnings = []
                                       , instructions = []
                                       , symbolTable = M.empty
                                       , offset = 0
                                       , lineNumber = 0 }


data Assembler m a where
  EmitError :: Error -> Assembler m ()
  EmitWarning :: Warning -> Assembler m ()
  EmitInstruction :: Ins.Instruction -> Assembler m ()
  BumpLineNumber :: Assembler m ()
  GetLineNumber :: Assembler m Int
  GetOffset :: Assembler m Int
  UpdateSymbol :: Text -> Symbol -> (Symbol -> Symbol -> Symbol) -> Assembler m ()
  GetSymbol :: Text -> Assembler m (Maybe Symbol)
  GetSymtab :: Assembler m Symtab
makeSem ''Assembler

assemble :: [P.AST] -> ([Warning], Either [Error] (Symtab, [Ins.Instruction]))
assemble = validateAssemblerState . semChain
  where semChain :: [P.AST] -> AssemblerState
        semChain = fst . run . runState initialAssemblerState . assemblerToState . runAssembler

validateAssemblerState :: AssemblerState -> ([Warning], Either [Error] (Symtab, [Ins.Instruction]))
validateAssemblerState AssemblerState{warnings, errors, symbolTable, instructions} =
  (warnings, if not . null $ errors then Left errors else Right (symbolTable, instructions))

assemblerToState :: Sem (Assembler ': r) a -> Sem (State AssemblerState ': r) a
assemblerToState = reinterpret \case
  EmitError e -> modify (\s@AssemblerState{errors} -> s{errors = e:errors})
  EmitWarning w -> modify (\s@AssemblerState{warnings} -> s{warnings = w:warnings})
  EmitInstruction i -> modify (\s@AssemblerState{instructions, offset} -> s{instructions = instructions ++ [i], offset = succ offset})
  BumpLineNumber -> modify (\s@AssemblerState{lineNumber} -> s{lineNumber = succ lineNumber})
  GetLineNumber -> gets lineNumber
  GetOffset -> gets offset
  UpdateSymbol name sym f -> modify (\s@AssemblerState{symbolTable} -> s{symbolTable = M.insertWith f name sym symbolTable})
  GetSymbol name -> gets (\AssemblerState{symbolTable} -> M.lookup name symbolTable)
  GetSymtab -> gets symbolTable

runAssembler :: [P.AST] -> Sem '[Assembler] ()
runAssembler ast = mapM_ (\l -> assembleLine l >> bumpLineNumber) ast >> validateSymtab
  where assembleLine :: P.AST -> Sem '[Assembler] ()
        assembleLine = \case
          (P.Instruction Nothing i) -> emitInstruction i
          (P.Instruction (Just s) i) -> do
            offset <- getOffset
            reference s offset
            assembleLine $ P.Instruction Nothing i
          (P.Command txt) -> parse txt >>= validate >>= execute

-- TODO: Support const
data Command = Comment Text
             | Label Text
             | Import Text
             | Export Text
             | Invalid Command
             | Unknown Text
             deriving (Eq, Show)

parse :: Text -> Sem '[Assembler] Command
parse t | T.isPrefixOf " " t = pure . Comment . T.tail $ t
        | T.isPrefixOf "label" t = pure . Label . dropCommandName $ t
        | T.isPrefixOf "import" t = pure . Import . dropCommandName $ t
        | T.isPrefixOf "export" t = pure . Export . dropCommandName $ t
        | otherwise = pure . Unknown $ t
        where dropCommandName = T.unwords . drop 1 . T.words

validate :: Command -> Sem '[Assembler] Command
validate c@(Comment _) = pure c
validate c@(Unknown _) = do
           line <- getLineNumber
           emitWarning $ UnknownCommand line
           pure c
validate c@(Label t) = argumentCountEqualsOrUnknown 1 t c
validate c@(Import t) = argumentCountEqualsOrUnknown 1 t c
validate c@(Export t) = argumentCountEqualsOrUnknown 1 t c
validate (Invalid _) = error "Got already Invalid command in validate"

argumentCountEqualsOrUnknown :: Int -> Text -> Command -> Sem '[Assembler] Command
argumentCountEqualsOrUnknown count txt cmd =
  if (length . T.words $ txt) == count
  then pure cmd
  else do line <- getLineNumber
          emitError $ InvalidNumberOfArguments line
          pure . Invalid $ cmd

execute :: Command -> Sem '[Assembler] ()
execute (Label name) = define name
execute (Import name) = markImported name
execute (Export name) = markExported name
execute _ = pure ()

reference :: Text -> Int -> Sem '[Assembler] ()
reference name line = updateSymbol name (emptySymbol{references = [line]})
    (\_ existing@Symbol{references} -> existing{references = line:references})

alreadyDefined :: Text -> Sem '[Assembler] Bool
alreadyDefined name = do
  sym <- getSymbol name
  case sym of
    Nothing -> pure False
    Just (Symbol{definedAt}) -> pure . isJust $ definedAt

define :: Text -> Sem '[Assembler] ()
define name = do
  offset <- getOffset
  defined <- alreadyDefined name
  if defined
     then do
       emitError $ DuplicateDefinition offset
     else do
       updateSymbol name (emptySymbol{definedAt = Just offset})
        (\_ existing -> existing{definedAt = Just offset})

markImported :: Text -> Sem '[Assembler] ()
markImported name = updateSymbol name (emptySymbol{imported = True})
  (\_ existing -> existing{imported = True})

markExported :: Text -> Sem '[Assembler] ()
markExported name = updateSymbol name (emptySymbol{exported = True})
  (\_ existing -> existing{exported = True})

-- TODO: can't get symtab here
validateSymtab :: Sem '[Assembler] ()
validateSymtab = do
  symtab <- getSymtab
  let (warnings, errors) = M.foldr checkSymbol ([], []) symtab
  mapM_ emitWarning warnings
  mapM_ emitError errors
    where checkSymbol :: Symbol -> ([Warning], [Error]) -> ([Warning], [Error])
          checkSymbol sym@Symbol{definedAt, references} (ws, es)
            | isUnreferenced sym = ((UnreferencedSymbol . fromJust $ definedAt):ws, es)
            | isUndefined sym = (ws, (Undefined . head $ references):es)
            | otherwise = (ws, es)
          isUnreferenced Symbol{references} = null references
          isUndefined Symbol{definedAt} = isNothing definedAt
