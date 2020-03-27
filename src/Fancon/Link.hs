module Fancon.Link (link) where

import Data.Text (Text)
import Fancon.Assemble (Module)
import Fancon.Symboltable
import Fancon.Instruction
import qualified Data.Map as M
import Data.Map ((\\))
import qualified Data.List.NonEmpty as NE
import Data.Array

data Error = Undefined Text
           | DuplicateDefinition Text
           | RelocationPropertyMismatch Text
           deriving (Eq, Show)

link :: [Module] -> Module
link = foldr merge (emptySymbolTable, array (1, 0) [])

merge :: Module -> Module -> Module
merge (aSymtab, aInstructions) (bSymtab, bInstructions) = (cSymtab, cInstructions)
  where cSymtab = emptySymbolTable{ exports = exports aResolvedSymtab <> exports bResolvedSymtab
                                  , imports = imports aResolvedSymtab <> imports bResolvedSymtab
                                  , references = M.unionWith (<>) (references aResolvedSymtab) (references bResolvedSymtab)
                                  , local = M.union (local aResolvedSymtab) (local bResolvedSymtab)
                                  }
        cInstructions = listArray (1, length aResolvedInstructions + length bResolvedInstructions) $ elems aResolvedInstructions ++ elems bResolvedInstructions
        (aResolvedSymtab, aResolvedInstructions) = resolveLocals (aSymtab `resolveImportsFrom` offsetSymbolTable (length aInstructions) bSymtab, aInstructions)
        (bResolvedSymtab, bResolvedInstructions) = resolveLocals (bSymtab `resolveImportsFrom` aResolvedSymtab, bInstructions)

resolveLocals :: Module -> Module
resolveLocals (symtab, instructions) = (symtab', instructions')
  where symtab' = symtab{ local = M.restrictKeys (local symtab) (exports symtab)
                        , references = references symtab \\ M.withoutKeys localReferences (exports symtab)
                        }
        localReferences = referencesToLocals symtab
        instructions' = M.foldrWithKey resolveSymbol instructions localReferences
        lookupSymbol name = value $ local symtab M.! name
        resolveSymbol name refs ins =
          -- TODO: Ugly AF
          ins // map (\(lineIx, opIx) -> (lineIx, resolveReference (ins ! lineIx) opIx (lookupSymbol name))) (NE.toList refs)

resolveReference :: Instruction -> OpIx -> Int -> Instruction
resolveReference i opIx v = i{operands = operands'}
  where operands' = replace (operands i) opIx op'
        op = operands i !! opIx
        op' = resolveOp op v
        resolveOp (Immediate _) newVal = Immediate (fromIntegral newVal)
        resolveOp (Register _) _ = error "Trying to resolve register"

replace :: [a] -> Int -> a -> [a]
replace list ix v = take ix list ++ [v] ++ drop (succ ix) list
