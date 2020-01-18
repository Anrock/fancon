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

data Symbol = Symbol { references :: [(Int, Int)]
                     , exported :: Bool
                     , imported :: Bool
                     , value :: Maybe Int
                     }
            deriving (Eq, Show)

emptySymbol :: Symbol
emptySymbol = Symbol { references = []
                     , exported = False
                     , imported = False
                     , value = Nothing
                     }

reference' :: Int -> Int -> Symbol -> Symbol
reference' line idx sym@Symbol{references} = sym{references = (line, idx):references}

define' :: Int -> Symbol -> Symbol
define' value sym = sym{value = Just value}

markImported' :: Symbol -> Symbol
markImported' sym = sym{imported = True}

markExported' :: Symbol -> Symbol
markExported' sym = sym{exported = True}

type Symtab = M.Map Text Symbol

emptySymtab :: Symtab
emptySymtab = M.empty

reference :: Text -> Int -> Int -> Symtab -> Symtab
reference name line idx symtab = M.insert name (reference' line idx (M.findWithDefault emptySymbol name symtab)) symtab

markImported :: Text -> Symtab -> Symtab
markImported name symtab = M.insert name (markImported' (M.findWithDefault emptySymbol name symtab)) symtab

markExported :: Text -> Symtab -> Symtab
markExported name symtab = M.insert name (markExported' (M.findWithDefault emptySymbol name symtab)) symtab

isDefined :: Text -> Symtab -> Bool
isDefined name symtab = case M.lookup name symtab of
    Nothing -> False
    Just Symbol{value} -> isJust value

define :: Text -> Int -> Symtab -> Symtab
define name val symtab = M.insert name (define' val (M.findWithDefault emptySymbol name symtab)) symtab

undefinedSymbols :: Symtab -> [(Text, Symbol)]
undefinedSymbols = M.toList . M.filter
  (\Symbol{value, imported} -> isNothing value && not imported)

unreferencedSymbols :: Symtab -> [(Text, Symbol)]
unreferencedSymbols = M.toList . M.filter
  (\Symbol{exported, references} -> null references && not exported)
