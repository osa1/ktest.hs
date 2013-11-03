{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import           Control.Concurrent.ParallelIO
import           Control.Monad.Error
import           Data.Maybe                    (fromMaybe)
import           GHC.Conc                      (getNumProcessors,
                                                setNumCapabilities)
import           Options.Applicative
import           System.Directory              (getCurrentDirectory,
                                                getDirectoryContents)
import           System.Exit                   (ExitCode (..), exitFailure,
                                                exitSuccess)
import           System.FilePath               (addExtension, dropExtension,
                                                takeDirectory, takeExtension,
                                                (</>))
import           System.IO                     (hGetContents, hPutStr)
import           System.Process

import           CmdArgs                       hiding (CmdArgs (..))
import           Types


run :: KTestOptions -> K ()
run opts@KTestOptions{..} = do
    -- TODO: thread numbers are ignored right now
    liftIO $ print opts
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
        ExitFailure _ -> do
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
        ExitFailure _ -> do
          errMsg <- hGetContents stderr
          putStrLn $ concat [ "kompile failed: ", errMsg, "\nargs were: ", show args ]
          return False
        ExitSuccess -> return True

runKRuns :: Bool -> Int -> Int -> [TestCase] -> K ()
runKRuns verbose _ timeout tests = do
    --results <- liftIO $ parallel $ map mkKrunProc tests
    ts <- liftM concat $ mapM collectCompFiles tests
    liftIO $ print ts
    rets <- liftIO $ parallel $ map mkKrunProc ts
    liftIO $ print rets
    return ()
    --liftIO $ print results
  where
    -- | Run program, passing stdin file as input,
    --     - if it returns 0, compare output with stdout file (if it exists)
    --     - if it returns non-zero, compare stderr with stderr file (same)
    -- TODO: what happens to stdout if program returned non-zero?
    mkKrunProc :: (FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath) -> IO Bool
    mkKrunProc (defPath, prog, stdinf, stdoutf, stderrf) = do
      let args = [prog, "--directory=" ++ takeDirectory defPath]
      putStrLn $ "creating krun process with args: " ++ show args
      (Just stdin, Just stdout, Just stderr, phandle) <-
        createProcess (proc "krun" args){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe}
      maybe (return ()) (\f -> hPutStr stdin =<< readFile f) stdinf
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure _ -> do
          err <- hGetContents stderr
          case stderrf of
            Nothing -> do
              putStrLn $ concat [ "krun failed: ", err, "\nargs were: ", show args ]
              return False
            Just f -> do
              errorFile <- readFile f
              return (errorFile == err)
        ExitSuccess -> do
          case stdoutf of
            Nothing -> return True
            Just f -> do
              outputFile <- readFile f
              output <- hGetContents stdout
              return $ outputFile == output

    checkExt :: String -> FilePath -> Bool
    --checkExt ext path | trace (show (ext, path)) False = undefined
    checkExt ext path = case takeExtension path of
                          '.' : rest -> ext == rest
                          _ -> False

    -- | Collect (definition path, program, stdin, stdout, stderr) file paths for test case,
    --   all paths should be absolute
    collectCompFiles :: TestCase -> K [(FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)]
    collectCompFiles tc@TestCase{definition=defPath, programs=programs, progFileExtension=ext} =
      case ext of
        Nothing   -> -- skip the test
                     return []
        Just ext' -> liftIO $ do
          liftM concat $ forM programs $ \p -> do
            dirContents <- getDirectoryContents p
            let progFiles = filter (checkExt ext') dirContents
            return $ map (\f ->
                         let stdinf  = dropExtension f `addExtension` ext' `addExtension` "in"
                             stdoutf = dropExtension f `addExtension` ext' `addExtension` "out"
                             stderrf = dropExtension f `addExtension` ext' `addExtension` "err"

                             stdinr  = if stdinf  `elem` dirContents then Just (p </> stdinf) else Nothing
                             stdoutr = if stdoutf `elem` dirContents then Just (p </> stdoutf) else Nothing
                             stderrr = if stderrf `elem` dirContents then Just (p </> stderrf) else Nothing
                          in (defPath, p </> f, stdinr, stdoutr, stderrr)) progFiles

-- TODO: show help message when it's run without arguments
main :: IO ()
main = do
    -- this should remove necessity of +RTS -Nn -RTS arguments and set n to
    -- number of processors
    getNumProcessors >>= setNumCapabilities

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
