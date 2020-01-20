module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Map as M
import Control.Monad

import Fancon.Parse
import Fancon.Assemble

main :: IO ()
main = do
  [fileName] <- getArgs
  fileContents <- T.readFile fileName

  case parse fileContents of
    Left e -> putStrLn . errorBundlePretty $ e
    Right ast -> do sequence_ $ print <$> zip [1..] ast
                    putStrLn ""

                    let (warnings, assembleResult) = assemble ast
                    unless (null warnings) $ do
                      putStrLn "Warnings: "
                      sequence_ (print <$> warnings)
                      putStrLn ""

                    case assembleResult of
                      Left errors -> putStrLn "Errors: " >> sequence_ (print <$> errors)
                      Right (symtab, instructions) -> do
                        unless (M.null symtab) $ do
                          putStrLn "Symbol table"
                          sequence_ (print <$> M.toList symtab)
                          putStrLn ""

                        putStrLn "Instructions: "
                        sequence_ (print <$> zip [1..] instructions)
                        putStrLn ""
