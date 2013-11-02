{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module CmdArgs where


import GHC.Conc (numCapabilities)
import Options.Applicative
import Control.Monad.Identity
import System.FilePath (takeDirectory)
import Data.Monoid


data CmdArgs m = CmdArgs
    { verbose     :: Bool
    , programs    :: m FilePath -- ^ programs directory in single mode, root folder for programs in batch mode
    , extension   :: Maybe String   -- ^ extension of programs to pass to krun
    , exclude     :: Maybe String   -- ^ names of excluded program names
    , results     :: m FilePath -- ^ directory that contains output files to compare output of krun
    , skip        :: m String
    , directory   :: m FilePath -- ^ directory where K definitions reside
    , threads     :: m Int
    , timeout     :: m Int
    , testFile    :: FilePath
    }

validate :: CmdArgs Maybe -> CmdArgs Identity
validate CmdArgs{..} =
    CmdArgs{ verbose
           , programs=programs'
           , extension
           , exclude
           , results=results'
           , skip=skip'
           , directory=directory'
           , threads=threads'
           , timeout=timeout'
           , testFile
           }
  where
    programs' = return $ maybe (takeDirectory testFile) id programs
    results' = return $ maybe (takeDirectory testFile) id results
    skip' = undefined
    directory' = return $ maybe "." id directory
    threads' = return $ maybe numCapabilities id threads
    timeout' = return $ maybe 100 id timeout

argParser :: Parser (CmdArgs Maybe)
argParser = CmdArgs
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> optional (strOption
      ( long "programs"
     <> metavar "dir"
     <> help "Programs directory in single job mode, or a root directory for programs in batch mode. By default this is the directory where <file> reside." ))
  <*> optional (strOption
      ( long "extension"
     <> metavar "string"
     <> help "The list of program extensions separated by whitespaces. Required in single job mode, invalid in batch mode." ))
  <*> optional (strOption
      ( long "exclude"
     <> metavar "file"
     <> help "The list of programs which will not be tested. Valid only in single job mode." ))
  <*> optional (strOption
      ( long "results"
     <> metavar "dir"
     <> help "Directory containing input and expected output for programs in single job mode, or a root directory for the expected I/O for programs in batch mode. By default this is the directory where <file> reside." ))
  <*> optional (strOption
      ( long "skip"
     <> metavar "steps"
     <> help "The list of steps separated by whitespace to be skipped. A step is either [kompile|pdf|programs]." ))
  <*> optional (strOption
      ( long "directory"
     <> short 'd'
     <> metavar "dir"
     <> help "A root directory where K definitions reside. By default this is the current directory. Valid only in batch mode." ))
  <*> optional (option -- TODO: option error messages are not helpful
      ( long "threads"
     <> metavar "num"
     <> help "Maximum number of threads." ))
  <*> optional (option
      ( long "timeout"
     <> metavar "num"
     <> help "Testing time limit (seconds)." ))
  <*> argument Just mempty
