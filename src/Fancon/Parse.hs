module Fancon.Parse (parse, AST(..)) where

import Prelude hiding (Word, div, or, and)
import Data.Text (Text, pack)
import Text.Megaparsec hiding (parse, label, Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

import Fancon.Memory (Word, Byte)
import qualified Fancon.Instruction as Ins

type Parser = Parsec Void Text

data AST = Command Text
         | Instruction (Maybe Text) Ins.Instruction
         deriving (Show, Eq)

-- * Helper parsers
spaceConsumer, spaceConsumer' :: Parser ()
spaceConsumer = L.space space1 empty empty
spaceConsumer' = L.space (char ' ' >> pure ()) empty empty

lexeme, lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme spaceConsumer'
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol s = L.symbol spaceConsumer' s >> pure ()

untilEOL :: Parser Text
untilEOL = takeWhileP Nothing (/= '\n')

identifier :: Parser Text
identifier = pack <$> some (alphaNumChar <|> oneOf ("!@#$%^&*-_" :: String))

-- * Top level parsers
parse :: Text -> Either (ParseErrorBundle Text Void) [AST]
parse = runParser assembly ""

assembly :: Parser [AST]
assembly = spaceConsumer >> some (command <|> instruction)

command :: Parser AST
command = lexeme $ char '.' >> Command <$> untilEOL

instruction :: Parser AST
instruction = lexeme $ choice ([ add
                               , sub
                               , div
                               , mul
                               , xor
                               , shf
                               , and
                               , or
                               , save
                               , load
                               , jmp
                               , jgz
                               , jlt
                               , jez
                               , int
                               , brk ] :: [Parser AST])

-- * Instruction parsers
-- ** Instruction parser helpers
register :: Parser Byte
register = lexeme' $ do
  registerPrefix
  -- TODO: report error if register isn't 0..7
  L.decimal <?> "register number"

registerPrefix :: Parser ()
registerPrefix = (symbol "r" <?> "register prefix") >> pure ()

immediateOrLabel :: Parser (Either Word Text)
immediateOrLabel = (Left <$> immediate) <|> (Right <$> label)

-- TODO: report error if immediate isn't 0..65535
immediate :: Parser Word
immediate = lexeme' L.decimal <?> "immediate value"

label :: Parser Text
label = lexeme' identifier <?> "label"

-- ** operand parsers
rrr :: Parser (Byte, Byte, Byte)
rrr = (,,) <$> register <*> register <*> register

insRRR
  :: (Byte -> Byte -> Byte -> Ins.Instruction) -- ^ instruction constructor
  -> Parser AST
insRRR inscons = do
  (a, b, c) <- rrr
  pure . Instruction Nothing $ inscons a b c

rir :: Parser (Byte, Either Word Text, Byte)
rir = (,,) <$> register <*> immediateOrLabel <*> register

insRRI
  :: (Byte -> Word -> Byte -> Ins.Instruction)
  -> Parser AST
insRRI inscons = do
  (a, b, c) <- rir
  pure $ either (resolvedRRI inscons a c) (unresolvedRRI inscons a c) b

resolvedRRI :: (Byte -> Word -> Byte -> Ins.Instruction) -> Byte -> Byte -> Word -> AST
resolvedRRI inscons a c w = Instruction Nothing $ inscons a w c

unresolvedRRI :: (Byte -> Word -> Byte -> Ins.Instruction) -> Byte -> Byte -> Text -> AST
unresolvedRRI inscons a c l = Instruction (Just l) $ inscons a 0 c

rr :: Parser (Byte, Byte)
rr = (,) <$> register <*> register

insRR :: (Byte -> Byte -> Ins.Instruction)
      -> Parser AST
insRR inscons = do
  (a, b) <- rr
  pure . Instruction Nothing $ inscons a b

ir :: Parser (Either Word Text, Byte)
ir = (,) <$> immediateOrLabel <*> register

insIR :: (Byte -> Word -> Ins.Instruction)
      -> Parser AST
insIR inscons = do
  (a, b) <- ir
  pure $ either (resolvedIR inscons b) (unresolvedIR inscons b) a

resolvedIR :: (Byte -> Word -> Ins.Instruction) -> Byte -> Word -> AST
resolvedIR inscons a b = Instruction Nothing $ inscons a b

unresolvedIR :: (Byte -> Word -> Ins.Instruction) -> Byte -> Text -> AST
unresolvedIR inscons a b = Instruction (Just b) $ inscons a 0

ri :: Parser (Byte, Either Word Text)
ri = (,) <$> register <*> immediateOrLabel

insRI :: (Byte -> Word -> Ins.Instruction)
      -> Parser AST
insRI inscons = do
  (a, b) <- ri
  pure $ either (resolvedRI inscons a) (unresolvedRI inscons a) b

resolvedRI :: (Byte -> Word -> Ins.Instruction) -> Byte -> Word -> AST
resolvedRI inscons a b = Instruction Nothing $ inscons a b

unresolvedRI :: (Byte -> Word -> Ins.Instruction) -> Byte -> Text -> AST
unresolvedRI inscons a b = Instruction (Just b) $ inscons a 0

-- TODO: Try to make combinator to make ins :: op -> operands -> (operands -> AST) -> AST
-- ** Individual instruction parsers
-- *** Arith instruction parsers
arithIns :: Text
      -> (Byte -> Byte -> Byte -> Ins.Instruction)
      -> (Byte -> Word -> Byte -> Ins.Instruction)
      -> Parser AST
arithIns op rConstr iConstr = symbol op >> try (insRRR rConstr) <|> insRRI iConstr

add, sub, div, mul, xor, shf, and, or :: Parser AST
add = arithIns "add" Ins.add Ins.addi
sub = arithIns "sub" Ins.sub Ins.subi
div = arithIns "div" Ins.div Ins.divi
mul = arithIns "mul" Ins.mul Ins.muli
xor = arithIns "xor" Ins.xor Ins.xori
shf = arithIns "shf" Ins.shf Ins.shfi
and = arithIns "and" Ins.and Ins.andi
or  = arithIns "or"  Ins.or  Ins.ori

-- *** Memory instruction parsers
save, saver, savei, load, loadr, loadi :: Parser AST
save = symbol "save" >> try saver <|> savei
saver = insRR Ins.save
savei = insIR Ins.savei

load = symbol "load" >> try loadr <|> loadi
loadr = insRR Ins.load
loadi = insRI (flip Ins.loadi)

-- *** Jump instruction parsers
jmp, jmpr, jmpi, jgz, jlt, jez :: Parser AST
jmp = symbol "jmp" >> jmpr <|> jmpi
jmpr = do
  a <- register
  pure . Instruction Nothing $ Ins.jmp a
jmpi = either (Instruction Nothing . Ins.jmpi)
              (\l -> Instruction (Just l) $ Ins.jmpi 0) <$> immediateOrLabel

jumpIns :: Text
        -> (Byte -> Byte -> Ins.Instruction)
        -> (Byte -> Word -> Ins.Instruction)
        -> Parser AST
jumpIns opname rcons icons = do
  symbol opname
  try (insRR rcons) <|> insRI icons

jgz = jumpIns "jgz" Ins.jgz Ins.jgzi
jlt = jumpIns "jlt" Ins.jlt Ins.jlti
jez = jumpIns "jez" Ins.jez Ins.jezi

-- *** Special instruction parsers
int, brk :: Parser AST
int = do
  symbol "int"
  pure $ Instruction Nothing Ins.int
brk = do
  symbol "brk"
  pure $ Instruction Nothing Ins.brk

