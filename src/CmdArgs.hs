{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards #-}

module CmdArgs where


import           Control.Monad.Error
import           Data.Monoid
import           GHC.Conc            (numCapabilities)
import           Options.Applicative
import           System.FilePath     (takeDirectory, takeExtension)

import           ConfigParser        (parseConfigFile)
import           Types               (KTestError (..), KTestOptions (..),
                                      TestCase (..), SkipOpt (..))


data CmdArgs = CmdArgs
    { verbose   :: Bool
    , programs  :: Maybe FilePath -- ^ programs directory in single mode, root folder for programs in batch mode
    , extension :: Maybe String   -- ^ extension of programs to pass to krun
    , exclude   :: Maybe String   -- ^ names of excluded program names
    , results   :: Maybe FilePath -- ^ directory that contains output files to compare output of krun
    , skip      :: Maybe String
    , directory :: Maybe FilePath -- ^ directory where K definitions reside
    , threads   :: Maybe Int
    , timeout   :: Maybe Int
    , testFile  :: FilePath
    }

validate :: CmdArgs -> ErrorT KTestError IO KTestOptions
validate CmdArgs{..} =
    case takeExtension testFile of
      ".xml" -> do
        testCases <- liftIO $ parseConfigFile testFile
        -- FIXME: all other cmd options are being ignored here, maybe throw
        -- an error or warning
        return KTestOptions{verbose=verbose, threads=threads', timeout=timeout', tests=testCases, skips=skips'}
      ".k"   -> do
        tc <- testCase
        return KTestOptions{verbose=verbose, threads=threads', timeout=timeout', tests=[tc], skips=skips'}
          where
            testCase = do
              ext <- extension'
              return $ TestCase
                          { definition=testFile
                          , programs=Just programs'
                          , progFileExtension=Just ext
                          , excludes=fmap words exclude
                          , results=results
                          , kompileOptions=[]
                          , programSpecificKRunOptions=[]
                          }
      ext    -> throwError $ InvalidTestFileFmtErr ext
  where
    threads' = maybe numCapabilities id threads
    timeout' = maybe 100 id timeout
    programs' = maybe (takeDirectory testFile) id programs
    extension' = case extension of
                   Nothing  -> throwError $ strMsg "--extension is required in single job mode"
                   Just ext -> return ext
    skips' =
      let ws = maybe [] words skip in
      if "kompile" `elem` ws then [SkipKompile] else []
        ++ if "pdf" `elem` ws then [SkipPdf] else []
        ++ if "krun" `elem` ws then [SkipKRun] else []

argParser :: Parser CmdArgs
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
