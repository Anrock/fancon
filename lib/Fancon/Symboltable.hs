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
  , exports
  , imports
  , local
  , references
  , referencesToLocals
  , isDefined
  -- ** Possible warnings
  , importNameCollisions
  , undefinedExports
  , undefinedLocals
  , unusedImports
  , unusedLocals

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

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set ((\\))
import Data.Text (Text)
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
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

localNameSet :: SymbolTable -> S.Set SymbolName
localNameSet SymbolTable{ local } = S.fromDistinctAscList (M.keys local)

referencesNameSet :: SymbolTable -> S.Set SymbolName
referencesNameSet SymbolTable { references } = S.fromDistinctAscList (M.keys references)

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

unusedLocals :: SymbolTable -> S.Set SymbolName
unusedLocals s = localNameSet s \\ referencesNameSet s \\ exports s

unusedImports :: SymbolTable -> S.Set SymbolName
unusedImports s@SymbolTable{ imports } = imports \\ referencesNameSet s

undefinedLocals :: SymbolTable -> S.Set SymbolName
undefinedLocals s = referencesNameSet s \\ localNameSet s

undefinedExports :: SymbolTable -> S.Set SymbolName
undefinedExports s@SymbolTable { exports } = exports \\ localNameSet s

importNameCollisions :: SymbolTable -> S.Set SymbolName
importNameCollisions s@SymbolTable { imports } = S.intersection (localNameSet s) imports

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

printSymbolTable :: SymbolTable -> String
printSymbolTable symtab =
  [i|Symbol table
     Exports: #{S.toList (exports symtab)}
     Imports: #{S.toList (imports symtab)}
     Locals: #{M.toList (local symtab)}
     References: #{M.toList (references symtab)}|]

