module Fancon.Parse (parse, AST(..), Operand(..)) where

import Prelude hiding (div, or, and)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (parse, label, Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import qualified Data.Vector as V
import Data.Word (Word8, Word16)

type Parser = Parsec Void Text

data Operand = Register Word8 | Immediate Word16 | Label Text deriving (Eq, Show)
data AST = Command Text Int
         | Instruction Text [Operand] Int
         deriving (Show, Eq)

-- * Helper parsers
spaceConsumer, whiteSpaceConsumer :: Parser ()
whiteSpaceConsumer = L.space space1 empty empty
spaceConsumer = L.space (void . char $ ' ') empty empty

lexeme, lineLexeme :: Parser a -> Parser a
lineLexeme = L.lexeme whiteSpaceConsumer
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

untilEOL :: Parser Text
untilEOL = takeWhileP Nothing (/= '\n')

identifier :: Parser Text
identifier = lexeme $ pack <$> some (alphaNumChar <|> oneOf ("!@#$%^&*-_" :: String))

parse :: Text -> Either (ParseErrorBundle Text Void) (V.Vector AST)
parse input = case parsedLines of
                Left e -> Left e
                Right ast -> Right $ V.fromList ast
  where parsedLines = runParser assembly "" input

assembly :: Parser [AST]
assembly = whiteSpaceConsumer >> some (command <|> instruction)

command :: Parser AST
command = lineLexeme $ char '.' >> Command <$> untilEOL <*> (unPos <$> sourceLine <$> getSourcePos)

instruction :: Parser AST
instruction = lineLexeme $ do
  Instruction <$> identifier <*> many operand <*> (unPos <$> sourceLine <$> getSourcePos)

operand :: Parser Operand
operand = register <|> immediate <|> label

register :: Parser Operand
register = lexeme $ do
  registerPrefix
  Register <$> L.decimal <?> "register number"

registerPrefix :: Parser ()
registerPrefix = void (symbol "r" <?> "register prefix")

immediate :: Parser Operand
immediate = lexeme $ Immediate <$> (L.decimal <?> "immediate value")

label :: Parser Operand
label = Label <$> identifier <?> "label"
