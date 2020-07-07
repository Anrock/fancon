module Fancon.AssembleAlt where

import Prelude hiding (span)
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)

import Fancon.Parse
import Fancon.Symboltable
import Fancon.Instruction
import Fancon.Location

data Error = DuplicateSymbolDefinition SymbolName
           | UndefinedSymbolReference SymbolName
           | InvalidWord Text
           | InvalidOpcode Text
           | InvalidOperands [ASTOperand]
  deriving (Show, Eq)

data Warning = UnknownCommand Text
             | UnreferencedSymbol SymbolName
  deriving (Show, Eq)

data AssemblerState = AssemblerState
  { symtab :: SymbolTable
  , instructions :: V.Vector Instruction
  , errors :: [Spanned Error]
  , warnings :: [Spanned Warning]
  }

type Module = (SymbolTable, V.Vector Instruction)

assemble :: V.Vector AST -> (SymbolTable, V.Vector Instruction)
assemble ast = (symtab, instructions)
  where AssemblerState{symtab, instructions} = V.ifoldr assembleLine initialState ast
        initialState = AssemblerState emptySymbolTable V.empty [] []

assembleLine :: Int -> AST -> AssemblerState -> AssemblerState
assembleLine ix (Command cmd) s@AssemblerState{symtab, instructions, errors, warnings} =
  if T.head (item cmd) == ' '
  then s
  else case T.words (item cmd) of
    ["label", l] -> if isDefined l symtab
      then addError (span cmd) (DuplicateSymbolDefinition l) s
      else s{symtab = addLocation l line symtab}
    ["export", l] -> s{symtab = addExport l symtab}
    ["import", l] -> s{symtab = addImport l symtab}
    ["const", l, sval] ->
      case (readMaybe . T.unpack $ sval) :: Maybe Int of
        (Just val) ->
          if val < 0 || val > 65535
          then addError (InvalidWord sval line) s
          else if isDefined l symtab
               then s{errors = errors ++ [DuplicateSymbolDefinition l line]}
               else s{symtab = addConstant l (fromIntegral val) symtab}
        Nothing -> s{errors = errors ++ [InvalidWord sval line]}
    _ -> s{warnings = warnings ++ [UnknownCommand cmd line]}
assembleLine ix (Instruction op opers) s@AssemblerState{symtab, instructions, errors, warnings} =
  case validateOpcode op of
    Nothing -> s{errors = errors ++ [InvalidOpcode op line]}
    Just opcode -> undefined
    -- Just opcode -> undefined
    --   where indexedOperands = zip [0..] opers
    --         rawOperands = foldr raw [] indexedOperands
    --         raw = \case
    --           (_, Register r)  -> Register r
    --           (_, Immediate i) -> Immediate i
    --           (opIx, Left l)  -> do let symtab' = addReference l (lineIx, opIx) symtab
    --                                 modify (\s -> s{symtab = symtab'})
    --                                 pure $ Immediate 0
    --   case validateInstruction opcode operands' of
    --     Just ins -> emitInstruction ins
    --     Nothing -> emitError $ InvalidOperands operands' line


addError :: (SourcePos, SourcePos) -> Error -> AssemblerState -> AssemblerState
addError span e s@AssemblerState{errors} = s{errors = errors ++ [e']}
  where e' = Spanned span e
