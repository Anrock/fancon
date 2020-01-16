module Fancon.Parse (parse, AST(..), Operand(..)) where

import Prelude hiding (Word, div, or, and)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (parse, label, Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import Data.Functor (($>))

import Fancon.Memory (Word, Byte)

type Parser = Parsec Void Text

data Operand = Register Byte | Immediate Word | Label Text deriving (Eq, Show)
data AST = Command Text
         | Instruction Text [Operand]
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

parse :: Text -> Either (ParseErrorBundle Text Void) [AST]
parse = runParser assembly ""

assembly :: Parser [AST]
assembly = whiteSpaceConsumer >> some (command <|> instruction)

command :: Parser AST
command = lineLexeme $ char '.' >> Command <$> untilEOL

instruction :: Parser AST
instruction = lineLexeme $ do
  op <- identifier
  operands <- many operand
  pure $ Instruction op operands

operand :: Parser Operand
operand = register <|> immediate <|> label

register :: Parser Operand
register = generalRegister <|> spRegister

generalRegister :: Parser Operand
generalRegister = lexeme $ do
  registerPrefix
  Register <$> L.decimal <?> "register number"

spRegister :: Parser Operand
spRegister = lexeme $ void (symbol "sp" <?> "stack pointer register") $> Register 255

registerPrefix :: Parser ()
registerPrefix = void (symbol "r" <?> "register prefix")

immediate :: Parser Operand
immediate = lexeme $ Immediate <$> (L.decimal <?> "immediate value")

label :: Parser Operand
label = Label <$> identifier <?> "label"
