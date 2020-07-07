module Fancon.AssembleAlt where

import Polysemy
import Polysemy.State

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)

import Fancon.Parse
import Fancon.Symboltable
import Fancon.Instruction
import Control.Monad (forM)

data Error = DuplicateSymbolDefinition SymbolName LineIx
           | UndefinedSymbolReference SymbolName LineIx
           | InvalidWord Text LineIx
           | InvalidOpcode Text LineIx
           | InvalidOperands [ASTOperand] LineIx
  deriving (Show, Eq)

data Warning = UnknownCommand Text LineIx
             | UnreferencedSymbol SymbolName LineIx
  deriving (Show, Eq)

data AssemblerState = AssemblerState
  { symtab :: SymbolTable
  , instructions :: V.Vector Instruction
  , errors :: [Error]
  , warnings :: [Warning]
  }

mkAssemblyState :: AssemblerState
mkAssemblyState = AssemblerState emptySymbolTable V.empty [] []

type Module = (SymbolTable, V.Vector Instruction)

assemble :: Foldable t => t AST -> Either [Error] ([Warning], Module)
assemble ast = if not . null $ errors then Left errors else Right (warnings, (symtab, instructions))
  where AssemblerState{symtab, instructions, warnings, errors} =
          run . execState mkAssemblyState $ mapM_ assembleLine ast

assembleLine :: AST -> Sem '[State AssemblerState] ()
assembleLine (Command cmd line) =
  if T.head cmd == ' '
  then pure ()
  else case T.words cmd of
    ["label", l] -> isDefined l <$> (gets symtab) >>= \case
                      True -> addError $ DuplicateSymbolDefinition l line
                      False -> modify (\s@AssemblerState{symtab} -> s{symtab = addLocation l line symtab})
    ["export", l] -> modify (\s@AssemblerState{symtab} -> s{symtab = addExport l symtab})
    ["import", l] -> modify (\s@AssemblerState{symtab} -> s{symtab = addImport l symtab})
    ["const", l, sval] ->
      case (readMaybe . T.unpack $ sval) :: Maybe Int of
        Nothing -> addError $ InvalidWord sval line
        (Just val) ->
          if val < 0 || val > 65535
          then addError (InvalidWord sval line)
          else isDefined l <$> (gets symtab) >>= \case
            True -> addError $ DuplicateSymbolDefinition l line
            False -> modify (\s@AssemblerState{symtab} -> s{symtab = addConstant l val symtab})
    _ -> modify (\s@AssemblerState{warnings} -> s{warnings = warnings ++ [UnknownCommand cmd line]})

assembleLine (Instruction op opers line) =
  case validateOpcode op of
    Nothing -> addError (InvalidOpcode op line)
    Just opcode -> do
      operands' <- forM (zip [0..] opers) raw

      case validateInstruction opcode operands' of
        Nothing -> addError (InvalidOperands opers line)
        Just ins -> modify (\s@AssemblerState{instructions} -> s{instructions = instructions <> V.singleton ins})
      where raw (_, Right (Register r)) = pure $ Register r
            raw (_, Right (Immediate i)) = pure $ Immediate i
            raw (opIx, Left l) = do
              modify (\s@AssemblerState{symtab} -> s{symtab = addReference l (line, opIx) symtab})
              pure $ Immediate 0

addError :: Error -> Sem '[State AssemblerState] ()
addError e = modify (\s@AssemblerState{errors} -> s{errors = errors ++ [e]})
