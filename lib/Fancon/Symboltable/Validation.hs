module Fancon.Symboltable.Validation
( importNameCollisions
, undefinedExports
, undefineds
, unusedImports
, unusedLocals
) where

import Data.Set ((\\))
import Data.Set qualified as S
import Data.Map qualified as M
import Data.List.NonEmpty (NonEmpty)

import Fancon.Symboltable

unusedLocals :: SymbolTable -> S.Set SymbolName
unusedLocals s = localNameSet s \\ referencesNameSet s \\ exports s

unusedImports :: SymbolTable -> S.Set SymbolName
unusedImports s = imports s \\ referencesNameSet s

undefineds :: SymbolTable -> M.Map SymbolName (NonEmpty SymbolReference)
undefineds s = M.restrictKeys (references s) $
  referencesNameSet s \\ localNameSet s \\ imports s

undefinedExports :: SymbolTable -> S.Set SymbolName
undefinedExports s = exports s \\ localNameSet s

importNameCollisions :: SymbolTable -> S.Set SymbolName
importNameCollisions s = S.intersection (localNameSet s) (imports s)
