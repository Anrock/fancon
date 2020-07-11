module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty)
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.String.Interpolate.IsString
import System.Exit (exitFailure)

import Fancon
import Fancon.Assemble hiding (Error, Warning)
import qualified Fancon.Assemble as Asm
import Fancon.Link hiding (Error, Warning)
import qualified Fancon.Link as Link
import Arguments

main :: IO ()
main = getArguments >>= \case
  Compile opts -> do
    assembled <- forM (files opts) $ \file -> do
      putStr [i|#{file}: reading... |]
      content <- T.readFile file
      putStr "parsing... "
      case parse content of
        Left e -> do
          putStrLn . errorBundlePretty $ e
          exitFailure
        Right parsed -> do
          putStr "assembling... "
          case assemble parsed of
            Left errors -> prettyPrintAssemblyErrors content errors >> exitFailure
            Right (warnings, m@(symtab, _)) ->
              do unless (null warnings) $ prettyPrintAssemblyWarnings content warnings
                 when (dumpSymbolTable opts) $ putStrLn (printSymbolTable symtab)
                 putStrLn "done!"
                 pure m
    putStr [i|#{output opts}: linking... |]
    case link assembled of
      Left errors -> prettyPrintLinkErrors errors >> exitFailure
      Right (warnings, (symtab, instructions)) -> do
        prettyPrintLinkWarnings warnings
        when (dumpSymbolTable opts) $ putStrLn (printSymbolTable symtab)
        putStr "emitting binary... "
        let binary = emit instructions
        putStr "writing... "
        BS.writeFile (output opts) binary
        putStrLn "done!"

printWithLineMention :: Text -> Int -> Text -> IO ()
printWithLineMention file lineIx text =
    putStrLn [i|\n#{lineIx}: #{T.lines file !! pred lineIx}\n\t#{text}|]

prettyPrintAssemblyWarnings :: Text -> [Asm.Warning] -> IO ()
prettyPrintAssemblyWarnings file warnings = forM_ warnings \case
  UnknownCommand txt lineIx -> printWithLineMention file lineIx
    [i|Unknown command #{txt}|]
  UnreferencedSymbol sym lineIx -> printWithLineMention file lineIx
    [i|Unreferenced symbol #{sym}|]

prettyPrintLinkWarnings :: [Link.Warning] -> IO ()
prettyPrintLinkWarnings warnings = forM_ warnings \case
  NoMain -> putStrLn [i|No main symbol exported|]
  Unused sym -> putStrLn [i|Unreferenced symbol #{sym}|]

prettyPrintAssemblyErrors :: Text -> [Asm.Error] -> IO ()
prettyPrintAssemblyErrors file errors = forM_ errors \case
  DuplicateSymbolDefinition sym lineIx -> printWithLineMention file lineIx
    [i|Duplicate symbol definition #{sym}|]
  UndefinedSymbolReference sym lineIx -> printWithLineMention file lineIx
    [i|Undefined symbol reference #{sym}|]
  InvalidWord word lineIx -> printWithLineMention file lineIx
    [i|Immediate must be in range 0-65535, got #{word}|]
  InvalidOpcode opcode lineIx -> printWithLineMention file lineIx
    [i|Invalid opcode #{opcode}|]
  InvalidOperands operands lineIx -> printWithLineMention file lineIx
    [i|Invalid operands for command: #{operands}|]

prettyPrintLinkErrors :: [Link.Error] -> IO ()
prettyPrintLinkErrors errors = forM_ errors \case
  Undefined sym -> putStrLn [i|Undefined symbol #{sym}|]
  DuplicateDefinition sym -> putStrLn [i|Duplicate definition of #{sym}|]
