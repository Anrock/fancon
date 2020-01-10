module Fancon.Symboltable
  ( Symbol(..)
  , Symtab
  , emptySymtab
  , define
  , isDefined
  , reference
  , markImported
  , markExported
  , undefinedSymbols
  , unreferencedSymbols
  ) where

import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe (isJust, isNothing)

data Symbol = Symbol { references :: [Int]
                     , exported :: Bool
                     , imported :: Bool
                     , definedAt :: Maybe Int
                     }
            deriving (Eq, Show)

emptySymbol :: Symbol
emptySymbol = Symbol { references = []
                     , exported = False
                     , imported = False
                     , definedAt = Nothing
                     }

reference' :: Int -> Symbol -> Symbol
reference' line sym@Symbol{references} = sym{references = line:references}

define' :: Int -> Symbol -> Symbol
define' ofs sym = sym{definedAt = Just ofs}

markImported' :: Symbol -> Symbol
markImported' sym = sym{imported = True}

markExported' :: Symbol -> Symbol
markExported' sym = sym{exported = True}

type Symtab = M.Map Text Symbol

emptySymtab :: Symtab
emptySymtab = M.empty

reference :: Text -> Int -> Symtab -> Symtab
reference name line symtab = M.insert name (reference' line (M.findWithDefault emptySymbol name symtab)) symtab

markImported :: Text -> Symtab -> Symtab
markImported name symtab = M.insert name (markImported' (M.findWithDefault emptySymbol name symtab)) symtab

markExported :: Text -> Symtab -> Symtab
markExported name symtab = M.insert name (markExported' (M.findWithDefault emptySymbol name symtab)) symtab

isDefined :: Text -> Symtab -> Bool
isDefined name symtab = case M.lookup name symtab of
    Nothing -> False
    Just Symbol{definedAt} -> isJust definedAt

define :: Text -> Int -> Symtab -> Symtab
define name ofs symtab = M.insert name (define' ofs (M.findWithDefault emptySymbol name symtab)) symtab

undefinedSymbols :: Symtab -> [(Text, Symbol)]
undefinedSymbols = M.toList . M.filter
  (\Symbol{definedAt, imported} -> isNothing definedAt && not imported)

unreferencedSymbols :: Symtab -> [(Text, Symbol)]
unreferencedSymbols = M.toList . M.filter
  (\Symbol{exported, references} -> null references && not exported)
