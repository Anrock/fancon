module Fancon.Symboltable
  ( Symbol(..)
  , SymbolName
  , SymbolTable

  , LineIx
  , OpIx
  , SymbolReference

  , exports
  , imports
  , local
  , references

  , emptySymbolTable

  , addConstant
  , addExport
  , addImport
  , addLocation
  , addReference

  , isDefined

  , importNameCollisions
  , undefinedExports
  , undefinedLocals
  , unusedImports
  , unusedLocals
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set ((\\))
import Data.Text (Text)
import Data.List.NonEmpty

type SymbolName = Text
data Symbol = Location { value :: Int }
            | Constant { value :: Int }
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
