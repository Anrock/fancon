module Fancon.Assemble (assemble) where

import Prelude hiding (lines)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.State
import Control.Monad (mapM_, forM_, forM)
import Text.Read (readMaybe)
import Data.Maybe (isJust)

import qualified Fancon.Instruction as Ins
import qualified Fancon.Parse as P
import Fancon.Symboltable

type SymbolName = Text
type LineNumber = Int
type CommandText = Text

data Warning = UnknownCommand CommandText LineNumber
             | UnreferencedSymbol SymbolName Symbol
  deriving (Show, Eq)

data Error = DuplicateSymbolDefinition SymbolName LineNumber
           | UndefinedSymbolReference SymbolName LineNumber
           | InvalidWord Text LineNumber
           | InvalidOpcode Text LineNumber
           | InvalidOperands [Ins.Operand] LineNumber
  deriving (Show, Eq)

data AssemblerState = AssemblerState { errors :: [Error]
                                     , warnings :: [Warning]
                                     , instructions :: [Ins.Instruction]
                                     , symbolTable :: Symtab
                                     , lineNumber :: Int
                                     , significantLineNumber :: Int
                                     }

initialAssemblerState :: AssemblerState
initialAssemblerState = AssemblerState { errors = []
                                       , warnings = []
                                       , instructions = []
                                       , symbolTable = emptySymtab
                                       , lineNumber = 1
                                       , significantLineNumber = 1}

assemble :: [P.AST] -> ([Warning], Either [Error] (Symtab, [Ins.Instruction]))
assemble = validateAssemblerState . semChain
  where semChain :: [P.AST] -> AssemblerState
        semChain = fst . run . runState initialAssemblerState . runAssembler

validateAssemblerState :: AssemblerState -> ([Warning], Either [Error] (Symtab, [Ins.Instruction]))
validateAssemblerState AssemblerState{warnings, errors, symbolTable, instructions} =
  (warnings, if not . null $ errors then Left errors else Right (symbolTable, instructions))

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

runAssembler :: [P.AST] -> Sem '[State AssemblerState] ()
runAssembler ast = mapM_ assembleLine ast >> validateSymtab
  where assembleLine :: P.AST -> Sem '[State AssemblerState] ()
        assembleLine = \case
          (P.Instruction opcode operands) -> instruction opcode operands >> bumpLineNumber >> bumpSignificantLineNumber
          (P.Command txt) -> command txt >> bumpLineNumber

instruction :: Text -> [P.Operand] -> Sem '[State AssemblerState] ()
instruction opcodeStr operands = do
  line <- gets lineNumber
  case Ins.validateOpcode opcodeStr of
    Nothing -> emitError $ InvalidOpcode opcodeStr line
    Just opcode -> do
      operands' <- forM operands $ \case
        P.Register r -> pure $ Ins.Register r
        P.Immediate i -> pure $ Ins.Immediate i
        P.Label l -> do (symtab, sigLine) <- (,) <$> gets symbolTable <*> gets significantLineNumber
                        let symtab' = reference l sigLine symtab
                        modify (\s -> s{symbolTable = symtab'})
                        pure $ Ins.Immediate 0
      case Ins.validateInstruction opcode operands' of
        Just ins -> emitInstruction ins
        Nothing -> emitError $ InvalidOperands operands' line

command :: Text -> Sem '[State AssemblerState] ()
command txt
  | T.head txt == ' ' = pure ()
  | otherwise =
      case T.words txt of
        ["label", l] -> do symtab <- gets symbolTable
                           line <- gets significantLineNumber
                           if isDefined l symtab
                           then emitError $ DuplicateSymbolDefinition l line
                           else do let symtab' = define l line symtab
                                   modify (\s -> s{symbolTable = symtab'})
        ["export", l] -> do symtab' <- markExported l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["import", l] -> do symtab' <- markImported l <$> gets symbolTable
                            modify (\s -> s{symbolTable = symtab'})
        ["const", l, sval] ->
          do symtab <- gets symbolTable
             line <- gets significantLineNumber
             if isDefined l symtab
             then emitError $ DuplicateSymbolDefinition l line
             else do let val = (readMaybe . T.unpack $ sval) :: Maybe Word
                     if isJust val
                     then do let symtab' = define l line symtab
                             modify (\s -> s{symbolTable = symtab'})
                     else emitError $ InvalidWord sval line
        _ -> do line <- gets lineNumber
                emitWarning $ UnknownCommand txt line


validateSymtab :: Member (State AssemblerState) r => Sem r ()
validateSymtab = do
  symtab <- gets symbolTable
  forM_ (unreferencedSymbols symtab) (\(name, sym) -> emitWarning $ UnreferencedSymbol name sym)
  forM_ (undefinedSymbols symtab) (\(name, Symbol{references}) -> forM_ references (emitError . UndefinedSymbolReference name))
