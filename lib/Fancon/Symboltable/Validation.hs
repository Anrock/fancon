module Fancon.Symboltable.Validation
( importNameCollisions
, undefinedExports
, undefineds
, unusedImports
, unusedLocals
) where

import Data.Set ((\\))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty)

import Fancon.Symboltable

localNameSet :: SymbolTable -> S.Set SymbolName
localNameSet SymbolTable{ local } = S.fromDistinctAscList (M.keys local)

referencesNameSet :: SymbolTable -> S.Set SymbolName
referencesNameSet SymbolTable { references } = S.fromDistinctAscList (M.keys references)

unusedLocals :: SymbolTable -> S.Set SymbolName
unusedLocals s = localNameSet s \\ referencesNameSet s \\ exports s

unusedImports :: SymbolTable -> S.Set SymbolName
unusedImports s@SymbolTable{ imports } = imports \\ referencesNameSet s

undefineds :: SymbolTable -> M.Map SymbolName (NonEmpty SymbolReference)
undefineds s = M.restrictKeys (references s) $
  referencesNameSet s \\ localNameSet s \\ imports s

undefinedExports :: SymbolTable -> S.Set SymbolName
undefinedExports s@SymbolTable { exports } = exports \\ localNameSet s

importNameCollisions :: SymbolTable -> S.Set SymbolName
importNameCollisions s@SymbolTable { imports } = S.intersection (localNameSet s) imports
