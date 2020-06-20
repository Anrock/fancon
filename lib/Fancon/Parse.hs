module Fancon.Parse (parse, AST(..), ASTOperand) where

import Prelude hiding (div, or, and)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (parse, label, Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import qualified Data.Vector as V

import Fancon.Memory
import Fancon.Instruction (Operand(..))

type Parser = Parsec Void Text

type ASTOperand = Either Text Operand

data AST = Command Text Int
         | Instruction Text [ASTOperand] Int
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
command = lineLexeme $ char '.' >> Command <$> untilEOL <*> (unPos . sourceLine <$> getSourcePos)

instruction :: Parser AST
instruction = lineLexeme $ do
  Instruction <$> identifier <*> many operand <*> (unPos . sourceLine <$> getSourcePos)

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
