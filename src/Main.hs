{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import           Control.Concurrent.ParallelIO
import           Control.Monad.Error           (runErrorT, strMsg, throwError)
import           Control.Monad.Identity        (runIdentity)
import           Control.Monad.Reader
import           Data.Maybe                    (fromJust, fromMaybe)
import           Options.Applicative
import           System.Directory              (doesDirectoryExist,
                                                doesFileExist,
                                                getCurrentDirectory,
                                                getDirectoryContents)
import           System.Exit                   (ExitCode (..), exitFailure,
                                                exitSuccess)
import           System.FilePath               (takeExtension)
import           System.IO
import           System.Process

import           CmdArgs                       hiding (CmdArgs (..))
import           Types

import           Debug.Trace                   (trace)


run :: KTestOptions -> K ()
run KTestOptions{..} = do
    -- TODO: thread numbers are ignored right now
    liftIO $ print skips
    ss <- if SkipKompile `elem` skips
            then return Nothing
            else liftM Just (runKompiles verbose threads tests)
    unless (SkipPdf `elem` skips) $ runPdfs verbose threads (fromMaybe tests ss)
    unless (SkipKRun `elem` skips) $ runKRuns verbose threads timeout (fromMaybe tests ss)
    return ()

runKompiles :: Bool -> Int -> [TestCase] -> K [TestCase]
runKompiles verbose _ tests = do
    --when verbose $ liftIO . putStrLn $ compiling
    results <- liftIO $ parallel $ map mkKompileProc tests
    liftIO $ print (map fst results)
    return (map snd results)
  where
    mkKompileProc :: TestCase -> IO (Bool, TestCase)
    mkKompileProc tc = do
      let args = [definition tc]
      putStrLn $ "creating kompile process with args: " ++ show args
      (_, _, Just stderr, phandle) <- createProcess (proc "kompile" args){std_err=CreatePipe}
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure i -> do
          errMsg <- hGetContents stderr
          putStrLn $ concat [ "kompile failed: ", errMsg, "\nargs were: ", show args ]
          return (False, tc)
        ExitSuccess -> return (True, tc)

runPdfs :: Bool -> Int -> [TestCase] -> K ()
runPdfs verbose _ tests = liftIO $ parallel_ $ map mkPdfProc tests
  where
    mkPdfProc :: TestCase -> IO Bool
    mkPdfProc tc = do
      let args = [definition tc, "--backend=pdf"]
      putStrLn $ "creating kompile process with args: " ++ show args
      (_, _, Just stderr, phandle) <- createProcess (proc "kompile" args){std_err=CreatePipe}
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure i -> do
          errMsg <- hGetContents stderr
          putStrLn $ concat [ "kompile failed: ", errMsg, "\nargs were: ", show args ]
          return False
        ExitSuccess -> return True

runKRuns :: Bool -> Int -> Int -> [TestCase] -> K ()
runKRuns = undefined

runTest :: Bool -> Int -> Int -> [SkipOpt] -> TestCase -> K ()
runTest verbose threads timeout skips testcase = do
    liftIO $ print testcase
    programTests <- liftIO $ do
      pfs <- progFiles
      liftM and $ mapM doesDirectoryExist pfs
    definitionTest <- liftIO $ doesFileExist (definition testcase)
    liftIO $ putStrLn (definition testcase)
    liftIO $ print [show programTests, show definitionTest]
  where
    progFiles = do
      trace ("programs testcase = " ++ show (programs testcase)) (return ())
      dirContents <- liftM concat $ mapM getDirectoryContents $ programs testcase
      return $ filter ((==) (fromJust (progFileExtension testcase)) . takeExtension) dirContents

-- TODO: show help message when it's run without arguments
main :: IO ()
main = do
    currentDir <- getCurrentDirectory
    args <- execParser (opts currentDir)
    ret  <- runErrorT (validate args >>= runK . run)
    case ret of
      Left err -> print err >> exitFailure
      Right () -> exitSuccess
  where
    opts currentDir = info (helper <*> argParser currentDir)
      ( fullDesc
     -- <> progDesc "ktest"
     -- <> header ""
      )
