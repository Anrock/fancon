module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty)
import Control.Monad
import Data.Array
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

import Fancon

main :: IO ()
main = getArgs >>= \case
  "compile":fileNames -> compile fileNames
  _                   -> putStrLn "Unknown command"

printHeader :: String -> IO ()
printHeader fileName = do putStrLn fileName
                          putStrLn (replicate (length fileName) '=')

compile :: [FilePath] -> IO ()
compile fileNames = do
  compiledFiles <- forM fileNames $ \fileName -> do
    printHeader fileName
    fileContents <- T.readFile fileName
    compileFile fileContents

  unless (any isNothing compiledFiles) $ do
    let compiledFiles' = fromJust <$> compiledFiles
    printHeader "Linked"
    let (symtab, ins) = link compiledFiles'
    putStrLn $ printSymbolTable symtab
    putStrLn $ printInstructions ins
    let binary = emit ins
    BS.writeFile "out.exe.fancon" binary

compileFile :: Text -> IO (Maybe Module)
compileFile fileContents =
    case parse fileContents of
      Left e -> do putStrLn . errorBundlePretty $ e
                   pure Nothing
      Right ast -> do sequence_ $ print <$> assocs ast
                      putStrLn ""

                      let (warnings, assembleResult) = assemble ast
                      unless (null warnings) $ do
                        putStrLn "Warnings: "
                        sequence_ (print <$> warnings)
                        putStrLn ""

                      case assembleResult of
                        Left errors -> do putStrLn "Errors: "
                                          sequence_ (print <$> errors)
                                          pure Nothing
                        Right m@(symtab, instructions) -> do
                          putStrLn $ printSymbolTable symtab
                          unless (null instructions) $ putStrLn $ printInstructions instructions
                          pure . Just $ m
