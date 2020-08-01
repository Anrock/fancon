module Fancon.Parse (parse, AST(..), ASTOperand) where

import Prelude hiding (div, or, and)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (parse, label, Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void (Void)
import Control.Monad (void)
import Data.Vector qualified as V

import Fancon.Instruction (Operand(..))
import Fancon.Location

type Parser = Parsec Void Text

type ASTOperand = Either Text Operand

data AST = Command (Spanned Text)
         | Instruction (Spanned Text) [Spanned ASTOperand]
         deriving (Show, Eq)

parse :: Text -> Either (ParseErrorBundle Text Void) (V.Vector AST)
parse input = case parsedLines of
                Left e -> Left e
                Right ast -> Right $ V.fromList ast
  where parsedLines = runParser assembly "" input

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

spanned :: Parser a -> Parser (Spanned a)
spanned p = do
  begin <- getSourcePos
  r <- p
  end <- getSourcePos
  pure (Spanned (begin, end) r)

-- ** Main parsers
assembly :: Parser [AST]
assembly = whiteSpaceConsumer >> some (command <|> instruction)

command :: Parser AST
command = lineLexeme $ char '.' >> Command <$> spanned untilEOL

instruction :: Parser AST
instruction = lineLexeme $ Instruction <$> (spanned identifier) <*> many (spanned operand)

operand :: Parser ASTOperand
operand = register <|> immediate <|> label

register :: Parser ASTOperand
register = lexeme $ do
  registerPrefix
  Right . Register <$> L.decimal <?> "register number"

registerPrefix :: Parser ()
registerPrefix = void (symbol "r" <?> "register prefix")

immediate :: Parser ASTOperand
immediate = lexeme $ Right . Immediate <$> (L.decimal <?> "immediate value")

label :: Parser ASTOperand
label = Left <$> identifier <?> "label"
