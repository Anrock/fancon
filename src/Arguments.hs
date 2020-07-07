module Arguments (CompileOptions(..), Command(..), getArguments) where

import Options.Applicative

newtype Command = Compile CompileOptions

data CompileOptions = CompileOptions
  { files :: [FilePath],
    output :: FilePath,
    dumpSymbolTable :: Bool
  }

compileOptionsParser :: Parser CompileOptions
compileOptionsParser = CompileOptions
  <$> some (argument str (metavar "INPUT"))
  <*> strOption (
           long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> value "out.exe.fancon"
        <> help "File name of compiled binary")
  <*> switch (long "dump-symbol-table" <> help "Print symbol tables when compiling")

compileParser :: Parser Command
compileParser = Compile <$> compileOptionsParser

commandParser :: Parser Command
commandParser = subparser (
  command "compile" ( info (compileParser <**> helper) ( progDesc "Compile fancon assembly" ) )
  )

getArguments :: IO Command
getArguments = execParser (info (commandParser <**> helper) (header "Fancon - a fantasy console" <> fullDesc))
