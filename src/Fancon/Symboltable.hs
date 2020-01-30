module Fancon.Symboltable
  ( Symbol(..)
  , Symtab
  , emptySymtab
  , define
  , isDefined
  , reference
  , markImported
  , markExported
  , unreferenced
  , applyOffset
  , imports
  , exports
  , undefineds
  , locals
  , mangle
  ) where

import qualified Data.Map as M
import Data.Map ((\\))
import Data.Text (Text)
import Data.Maybe (isJust, isNothing)

data Symbol = Symbol { references :: [(Int, Int)]
                     , exported :: Bool
                     , imported :: Bool
                     , value :: Maybe Int
                     , relocatable :: Bool
                     }
            deriving (Eq, Show)

emptySymbol :: Symbol
emptySymbol = Symbol { references = []
                     , exported = False
                     , imported = False
                     , value = Nothing
                     , relocatable = False
                     }

reference' :: Int -> Int -> Symbol -> Symbol
reference' line idx sym@Symbol{references} = sym{references = (line, idx):references}

define' :: Int -> Bool -> Symbol -> Symbol
define' value reloc sym = sym{value = Just value, relocatable = reloc}

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

define :: Text -> Int -> Bool -> Symtab -> Symtab
define name val reloc symtab = M.insert name (define' val reloc (M.findWithDefault emptySymbol name symtab)) symtab

unreferenced :: Symtab -> Symtab
unreferenced = M.filter (null . references)

imports :: Symtab -> Symtab
imports = M.filter imported

exports :: Symtab -> Symtab
exports = M.filter exported

locals :: Symtab -> Symtab
locals s = s \\ imports s \\ exports s

undefineds :: Symtab -> Symtab
undefineds = M.filter (isNothing . value)

applyOffset :: Int -> Symtab -> Symtab
applyOffset ofs = fmap (\s@Symbol{relocatable, value, references} ->
  if not relocatable
  then s
  else s{ value = fmap (+ ofs) value
        , references = fmap (\(l, i) -> (l + ofs, i)) references })

mangle :: Text -> Symtab -> Symtab
mangle prefix = M.foldrWithKey mangle' M.empty
  where mangle' name s@Symbol{exported, imported} mangled =
          if imported || exported
             then M.insert name s mangled
             else M.insert (mconcat [prefix, "_", name]) s mangled

