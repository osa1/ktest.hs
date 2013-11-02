{-# OPTIONS_GHC -Wall #-}

module CmdArgs where


import Options.Applicative
import Data.Monoid


data CmdArgs = CmdArgs
    { verbose :: Bool
    , programs :: FilePath
    , extensions :: String
    , exclude :: String
    , results :: FilePath
    , skip :: String
    , directory :: FilePath
    , threads :: Int
    , timeout :: Int
    , testFile :: FilePath
    } deriving (Show)


argParser :: Parser CmdArgs
argParser = CmdArgs
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> strOption
      ( long "programs"
     <> metavar "dir"
     <> help "Programs directory in single job mode, or a root directory for programs in batch mode. By default this is the directory where <file> reside." )
  <*> strOption
      ( long "extension"
     <> metavar "string"
     <> help "The list of program extensions separated by whitespaces. Required in single job mode, invalid in batch mode." )
  <*> strOption
      ( long "exclude"
     <> metavar "file"
     <> help "The list of programs which will not be tested. Valid only in single job mode." )
  <*> strOption
      ( long "results"
     <> metavar "dir"
     <> help "Directory containing input and expected output for programs in single job mode, or a root directory for the expected I/O for programs in batch mode. By default this is the directory where <file> reside." )
  <*> strOption
      ( long "skip"
     <> metavar "steps"
     <> help "The list of steps separated by whitespace to be skipped. A step is either [kompile|pdf|programs]." )
  <*> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "dir"
     <> help "A root directory where K definitions reside. By default this is the current directory. Valid only in batch mode." )
  <*> option -- TODO: option error messages are not helpful
      ( long "threads"
     <> metavar "num"
     <> help "Maximum number of threads." )
  <*> option
      ( long "timeout"
     <> metavar "num"
     <> help "Testing time limit (seconds)." )
  <*> argument Just mempty
