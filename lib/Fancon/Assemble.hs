module Fancon.Assemble (assemble, SymbolTable, Module, Warning(..), Error(..)) where

import Prelude hiding (lines, const)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.State
import Control.Monad (forM)
import Text.Read (readMaybe)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import qualified Fancon.Instruction as Ins
import qualified Fancon.Parse as P
import Fancon.Symboltable (SymbolTable, LineIx, SymbolName)
import qualified Fancon.Symboltable.Validation as Sym
import qualified Fancon.Symboltable as Sym

type CommandText = Text
type Module = (SymbolTable, V.Vector Ins.Instruction)

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
                                     , instructions :: V.Vector Ins.Instruction
                                     , symbolTable :: SymbolTable
                                     , locations :: M.Map SymbolName LineIx
                                     , significantLineNumber :: Int
                                     }

initialAssemblerState :: AssemblerState
initialAssemblerState = AssemblerState { errors = []
                                       , warnings = []
                                       , instructions = []
                                       , symbolTable = Sym.emptySymbolTable
                                       , locations = M.empty
                                       , significantLineNumber = 0}

assemble :: Traversable t => t P.AST -> Either [Error] ([Warning], Module)
assemble = validateAssemblerState . semChain
  where semChain :: Traversable t => t P.AST -> AssemblerState
        semChain = fst . run . runState initialAssemblerState . runAssembler

validateAssemblerState :: AssemblerState -> Either [Error] ([Warning], Module)
validateAssemblerState AssemblerState{warnings, errors, symbolTable, instructions, locations} =
  case validateSymtab locations symbolTable of
    Left symErrors -> Left $ errors <> symErrors
    Right symWarns ->
      if not . null $ errors
      then Left errors
      else Right (warnings <> symWarns, (symbolTable, instructions))

validateSymtab :: M.Map SymbolName LineIx -> SymbolTable -> Either [Error] [Warning]
validateSymtab locs s = if not . null $ errors
                        then Left errors
                        else Right warnings
  where importCollisions = toDuplicate <$> (S.toList . Sym.importNameCollisions $ s)
        undefinedExports = toUndefined <$> (S.toList . Sym.undefinedExports $ s)
        undefineds = concat . M.elems . M.mapWithKey
                            (\sym refs -> NE.toList $ UndefinedSymbolReference sym <$> NE.map ((+1) . fst)  refs)
                              $ Sym.undefineds s
        unusedLocals =  toUnreferenced <$> (S.toList . Sym.unusedLocals $ s)
        unusedImports = toUnreferenced <$> (S.toList . Sym.unusedImports $ s)

        toUndefined sym = UndefinedSymbolReference sym (locs M.! sym)
        toUnreferenced sym = UnreferencedSymbol sym (locs M.! sym)
        toDuplicate sym = DuplicateSymbolDefinition sym (Sym.value $ Sym.local s M.! sym)

        errors = undefinedExports <> undefineds <> importCollisions
        warnings = unusedLocals <> unusedImports

emitError :: Member (State AssemblerState) r => Error -> Sem r ()
emitError e = modify (\s@AssemblerState{errors} -> s{errors = e:errors})

emitWarning :: Member (State AssemblerState) r => Warning -> Sem r ()
emitWarning w = modify (\s@AssemblerState{warnings} -> s{warnings = w:warnings})

emitInstruction :: Member (State AssemblerState) r => Ins.Instruction -> Sem r ()
emitInstruction i = modify (\s@AssemblerState{instructions} -> s{instructions = V.snoc instructions i})

bumpSignificantLineNumber :: Member (State AssemblerState) r => Sem r ()
bumpSignificantLineNumber = modify (\s@AssemblerState{significantLineNumber} -> s{significantLineNumber = succ significantLineNumber})

runAssembler :: Traversable t => t P.AST -> Sem '[State AssemblerState] ()
runAssembler = mapM_ assembleLine
  where assembleLine :: P.AST -> Sem '[State AssemblerState] ()
        assembleLine = \case
          (P.Instruction opcode operands pos) -> instruction opcode operands pos >> bumpSignificantLineNumber
          (P.Command txt pos) -> command txt pos

instruction :: Text -> [P.Operand] -> Int -> Sem '[State AssemblerState] ()
instruction opcodeStr operands line =
  case Ins.validateOpcode opcodeStr of
    Nothing -> emitError $ InvalidOpcode opcodeStr line
    Just opcode -> do
      operands' <- forM (zip [0..] operands) $ \case
        (_, P.Register r)  -> pure $ Ins.Register r
        (_, P.Immediate i) -> pure $ Ins.Immediate i
        (opIx, P.Label l)  -> do (symtab, lineIx) <- (,) <$> gets symbolTable <*> gets significantLineNumber
                                 let symtab' = Sym.addReference l (lineIx, opIx) symtab
                                 modify (\s -> s{symbolTable = symtab'})
                                 pure $ Ins.Immediate 0
      case Ins.validateInstruction opcode operands' of
        Just ins -> emitInstruction ins
        Nothing -> emitError $ InvalidOperands operands' line

command :: Text -> Int -> Sem '[State AssemblerState] ()
command txt line
  | T.head txt == ' ' = pure ()
  | otherwise =
      case T.words txt of
        ["label", l] -> label l line
        ["export", l] -> do symtab' <- Sym.addExport l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["import", l] -> do symtab' <- Sym.addImport l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["const", l, sval] -> const l sval line
        _ -> emitWarning $ UnknownCommand txt line

label :: Text -> Int -> Sem '[State AssemblerState] ()
label lbl line = do location <- gets significantLineNumber
                    defineFirstOrError lbl line (Sym.addLocation lbl location)

const :: Text -> Text -> Int -> Sem '[State AssemblerState] ()
const l sval line = case (readMaybe . T.unpack $ sval) :: Maybe Word of
                      (Just val) -> defineFirstOrError l line (Sym.addConstant l (fromIntegral val))
                      Nothing    -> emitError $ InvalidWord sval line

defineFirstOrError :: SymbolName -> Int -> (SymbolTable -> SymbolTable) -> Sem '[State AssemblerState] ()
defineFirstOrError name line f =
  do symtab <- gets symbolTable
     if Sym.isDefined name symtab
     then emitError $ DuplicateSymbolDefinition name line
     else do locations <- gets locations
             modify (\s -> s{symbolTable = f symtab, locations = M.insert name line locations})
