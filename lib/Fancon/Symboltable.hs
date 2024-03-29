-- TODO: shrink API, provide only basic combinators
module Fancon.Symboltable
  ( -- * Types and constructors
    Symbol(..)
  , SymbolTable
  , emptySymbolTable

  -- * Convenience type synonyms
  , SymbolName
  , LineIx
  , OpIx
  , SymbolReference

  -- * Queries
  , referencesToLocals
  , local
  , exports
  , imports
  , references
  , isDefined
  , localNameSet
  , referencesNameSet

  -- * Modification
  , addConstant
  , addExport
  , addImport
  , addLocation
  , addReference
  -- ** Offsetting
  , offsetSymbolTable
  , offsetSymbol
  , offsetReference
  -- ** Resolving
  , resolveImportsFrom

  , printSymbolTable
  ) where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set ((\\))
import Data.Text (Text)
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate

type SymbolName = Text
data Symbol = Location { value :: Int }
            -- ^ Code location, have to be offsetted depending on code location
            | Constant { value :: Int }
            -- ^ Constant value, like @timeout = 1 * 60@, doesn't have to be offsetted
            deriving (Eq, Show)

type LineIx = Int
type OpIx = Int
type SymbolReference = (LineIx, OpIx)

data SymbolTable = SymbolTable { local :: M.Map SymbolName Symbol
                               , exports :: S.Set SymbolName
                               , imports :: S.Set SymbolName
                               , references :: M.Map SymbolName (NonEmpty SymbolReference)
                               } deriving (Eq, Show)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable { local = M.empty
                               , exports = S.empty
                               , imports = S.empty
                               , references = M.empty
                               }

addReference :: SymbolName -> SymbolReference -> SymbolTable -> SymbolTable
addReference sym ref symtab = symtab{references = references'}
  where references' = M.insertWith (<>) sym (Data.List.NonEmpty.fromList [ref]) (references symtab)

addExport :: SymbolName -> SymbolTable -> SymbolTable
addExport sym s = s{exports = exports'}
  where exports' = S.insert sym (exports s)

addImport :: SymbolName -> SymbolTable -> SymbolTable
addImport sym s = s{imports = imports'}
  where imports' = S.insert sym (imports s)

addLocation :: SymbolName -> Int -> SymbolTable -> SymbolTable
addLocation sym val s = s{local = local'}
  where local' = M.insert sym (Location val) (local s)

addConstant :: SymbolName -> Int -> SymbolTable -> SymbolTable
addConstant sym val s = s{local = local'}
  where local' = M.insert sym (Constant val) (local s)

isDefined :: SymbolName -> SymbolTable -> Bool
isDefined sym s = M.member sym (local s)

offsetSymbolTable :: Int -> SymbolTable -> SymbolTable
offsetSymbolTable ofs s = s{local = local', references = references'}
  where local' = M.map (offsetSymbol ofs) (local s)
        references' = M.map (offsetReference ofs) (references s)

offsetSymbol :: Int -> Symbol -> Symbol
offsetSymbol ofs l@Location{value} = l{value = value + ofs}
offsetSymbol _ c = c

offsetReference :: Int -> NonEmpty SymbolReference -> NonEmpty SymbolReference
offsetReference ofs = NE.map (\(l, o) -> (l + ofs, o))

resolveImportsFrom :: SymbolTable -> SymbolTable -> SymbolTable
a `resolveImportsFrom` b = a{imports = imports a \\ namesToResolve, local = M.union (local a) symbolsToResolve}
  where namesToResolve = S.intersection (imports a) (exports b)
        symbolsToResolve = M.restrictKeys (local b) namesToResolve

referencesToLocals :: SymbolTable -> M.Map SymbolName (NonEmpty SymbolReference)
referencesToLocals SymbolTable{local, references} = M.intersection references local

localNameSet :: SymbolTable -> S.Set SymbolName
localNameSet = S.fromDistinctAscList . M.keys . local

referencesNameSet :: SymbolTable -> S.Set SymbolName
referencesNameSet = S.fromDistinctAscList . M.keys . references

printSymbolTable :: SymbolTable -> String
printSymbolTable symtab =
  [i|Symbol table
     Exports: #{S.toList (exports symtab)}
     Imports: #{S.toList (imports symtab)}
     Locals: #{M.toList (local symtab)}
     References: #{M.toList (references symtab)}|]

