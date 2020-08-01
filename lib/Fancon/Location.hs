module Fancon.Location 
  ( Pos
  , SourcePos(..)
  , Spanned(..)
  ) where

import Text.Megaparsec.Pos (SourcePos(..), Pos)

data Spanned a = Spanned {
  span :: (SourcePos, SourcePos),
  item :: a
} deriving (Eq, Show)

-- data AST = Command (Spanned Text)
--          | Instruction (Spanned Text) [(Spanned Text)]
--          deriving (Eq, Show)
--
-- data Module = Module {
--   code :: BS.ByteString,
--   symbolTable :: SymbolTable,
--   debugInfo :: M.Map Symbol (Spanned AST) -- Spanned AST?
-- }
--
-- data CompileError = InvalidCommand
--
-- data CompileWarning = SomeWarning
--
-- data LinkerError = UndefinedSymbol
--
-- data LinkerWarning = UnusedExport
--
-- parse :: Text -> V.Vector (Spanned AST)
-- parse = undefined
--
-- compile :: (Traversable t) => t (Spanned AST) -> Either [Spanned CompileError] ([Spanned CompileWarning], Module)
-- compile = undefined
--
-- link :: (Traversable t) => t Module -> Either [Spanned LinkerError] ([Spanned LinkerWarning], Module)
-- link = undefined
--
-- exe :: Module -> BS.ByteString
-- exe = undefined
