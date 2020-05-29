module Fancon.Assemble (assemble, SymbolTable, Module) where

import Prelude hiding (lines, const)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.State
import Control.Monad (forM)
import Text.Read (readMaybe)
import Data.Array
import qualified Data.Map as M

import qualified Fancon.Instruction as Ins
import qualified Fancon.Parse as P
import Fancon.Symboltable

type CommandText = Text
type Module = (SymbolTable, Array Int Ins.Instruction)

data Warning = UnknownCommand CommandText LineIx
             | UnreferencedSymbol SymbolName LineIx
  deriving (Show, Eq)

data Error = DuplicateSymbolDefinition SymbolName LineIx
           | UndefinedSymbolReference SymbolName LineIx
           | InvalidWord Text LineIx
           | InvalidOpcode Text LineIx
           | InvalidOperands [Ins.Operand] LineIx
  deriving (Show, Eq)

data AssemblerState = AssemblerState { errors :: [Error]
                                     , warnings :: [Warning]
                                     , instructions :: [Ins.Instruction]
                                     , symbolTable :: SymbolTable
                                     , locations :: M.Map SymbolName LineIx
                                     , lineNumber :: Int
                                     , significantLineNumber :: Int
                                     }

initialAssemblerState :: AssemblerState
initialAssemblerState = AssemblerState { errors = []
                                       , warnings = []
                                       , instructions = []
                                       , symbolTable = Sym.emptySymbolTable
                                       , locations = M.empty
                                       , lineNumber = 1
                                       , significantLineNumber = 1}

assemble :: Array Int P.AST -> Either [Error] ([Warning], Module)
assemble = validateAssemblerState . semChain
  where semChain :: Array Int P.AST -> AssemblerState
        semChain = fst . run . runState initialAssemblerState . runAssembler

validateAssemblerState :: AssemblerState -> Either [Error] ([Warning], Module)
validateAssemblerState AssemblerState{warnings, errors, symbolTable, instructions} =
  if not . null $ errors
  then Left errors
  else Right (warnings, (symbolTable, listArray (1, length instructions) instructions))

emitErrorAtCurrentLine :: Member (State AssemblerState) r => (LineIx -> Error) -> Sem r ()
emitErrorAtCurrentLine e = do line <- gets significantLineNumber
                              let err = e line
                              emitError err

emitError :: Member (State AssemblerState) r => Error -> Sem r ()
emitError e = modify (\s@AssemblerState{errors} -> s{errors = e:errors})

emitWarning :: Member (State AssemblerState) r => Warning -> Sem r ()
emitWarning w = modify (\s@AssemblerState{warnings} -> s{warnings = w:warnings})

emitInstruction :: Member (State AssemblerState) r => Ins.Instruction -> Sem r ()
emitInstruction i = modify (\s@AssemblerState{instructions} -> s{instructions = instructions ++ [i]})

bumpLineNumber :: Member (State AssemblerState) r => Sem r ()
bumpLineNumber = modify (\s@AssemblerState{lineNumber} -> s{lineNumber = succ lineNumber})

bumpSignificantLineNumber :: Member (State AssemblerState) r => Sem r ()
bumpSignificantLineNumber = modify (\s@AssemblerState{significantLineNumber} -> s{significantLineNumber = succ significantLineNumber})

runAssembler :: Array Int P.AST -> Sem '[State AssemblerState] ()
runAssembler = mapM_ assembleLine
  where assembleLine :: P.AST -> Sem '[State AssemblerState] ()
        assembleLine = \case
          (P.Instruction opcode operands) -> instruction opcode operands >> bumpLineNumber >> bumpSignificantLineNumber
          (P.Command txt) -> command txt >> bumpLineNumber

instruction :: Text -> [P.Operand] -> Sem '[State AssemblerState] ()
instruction opcodeStr operands =
  case Ins.validateOpcode opcodeStr of
    Nothing -> emitErrorAtCurrentLine (InvalidOpcode opcodeStr)
    Just opcode -> do
      operands' <- forM (zip [0..] operands) $ \case
        (_, P.Register r)  -> pure $ Ins.Register r
        (_, P.Immediate i) -> pure $ Ins.Immediate i
        (opIx, P.Label l)  -> do (symtab, lineIx) <- (,) <$> gets symbolTable <*> gets significantLineNumber
                                 let symtab' = addReference l (lineIx, opIx) symtab
                                 modify (\s -> s{symbolTable = symtab'})
                                 pure $ Ins.Immediate 0
      case Ins.validateInstruction opcode operands' of
        Just ins -> emitInstruction ins
        Nothing -> emitErrorAtCurrentLine (InvalidOperands operands')

command :: Text -> Sem '[State AssemblerState] ()
command txt
  | T.head txt == ' ' = pure ()
  | otherwise =
      case T.words txt of
        ["label", l] -> label l
        ["export", l] -> do symtab' <- addExport l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["import", l] -> do symtab' <- addImport l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["const", l, sval] -> const l sval
        _ -> do line <- gets lineNumber
                emitWarning $ UnknownCommand txt line

label :: Text -> Sem '[State AssemblerState] ()
label l = do location <- gets significantLineNumber
             defineFirstOrError l (addLocation l location)

const :: Text -> Text -> Sem '[State AssemblerState] ()
const l sval = case (readMaybe . T.unpack $ sval) :: Maybe Word of
                 (Just val) -> defineFirstOrError l (addConstant l (fromIntegral val))
                 Nothing    -> emitErrorAtCurrentLine (InvalidWord sval)

defineFirstOrError :: SymbolName -> (SymbolTable -> SymbolTable) -> Sem '[State AssemblerState] ()
defineFirstOrError name f = do symtab <- gets symbolTable
                               if isDefined name symtab
                               then emitErrorAtCurrentLine (DuplicateSymbolDefinition name)
                               else do line <- gets lineNumber
                                       locations <- gets locations
                                       modify (\s -> s{symbolTable = f symtab, locations = M.insert name line locations})
