module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Array

import Fancon.Parse
import Fancon.Assemble
import Fancon.Symboltable

main :: IO ()
main = getArgs >>= \case
  "compile":fileNames -> compile fileNames
  _                   -> putStrLn "Unknown command"

compile :: [FilePath] -> IO ()
compile fileNames =
  forM_ fileNames $ \fileName -> do
    putStrLn fileName
    putStrLn (replicate (length fileName) '=')
    fileContents <- T.readFile fileName
    case parse fileContents of
      Left e -> putStrLn . errorBundlePretty $ e
      Right ast -> do sequence_ $ print <$> assocs ast
                      putStrLn ""

                      let (warnings, assembleResult) = assemble ast
                      unless (null warnings) $ do
                        putStrLn "Warnings: "
                        sequence_ (print <$> warnings)
                        putStrLn ""

                      case assembleResult of
                        Left errors -> putStrLn "Errors: " >> sequence_ (print <$> errors)
                        Right (symtab, instructions) -> do
                          printSymbolTable symtab

                          unless (null instructions) $ do
                            putStrLn "Instructions: "
                            sequence_ (print <$> assocs instructions)
                            putStrLn ""

printSymbolTable :: SymbolTable -> IO ()
printSymbolTable symtab =
  do putStr "Symbol table"
     putStrLn "Exports:"
     sequence_ $ print <$> S.toList (exports symtab)
     putStrLn "Imports:"
     sequence_ $ print <$> S.toList (imports symtab)
     putStrLn "Locals:"
     sequence_ $ print <$> M.toList (local symtab)

