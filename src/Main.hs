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
                                                takeFileName, (</>))
import           System.IO                     (hGetContents, hPutStr)
import           System.Process

import           CmdArgs                       hiding (CmdArgs (..))
import           Types


run :: KTestOptions -> K ()
run opts@KTestOptions{..} = do
    -- TODO: thread numbers are ignored right now
    --liftIO $ print opts
    ss <- if SkipKompile `elem` skips
            then do
              liftIO $ putStrLn "Skipping kompile step."
              return Nothing
            else liftM Just (runKompiles verbose threads tests)
    if SkipPdf `elem` skips
      then liftIO $ putStrLn "Skipping pdf step."
      else runPdfs verbose threads (fromMaybe tests ss)
    if SkipKRun `elem` skips
      then liftIO $ putStrLn "Skipping krun step."
      else runKRuns verbose threads timeout (fromMaybe tests ss)
    return ()

-- TODO: This function assumes every definition has at most one test case,
-- is it really the case?
runKompiles :: Bool -> Int -> [TestCase] -> K [TestCase]
runKompiles verbose _ tests = do
    --when verbose $ liftIO . putStrLn $ compiling
    liftIO . putStrLn $ concat [ "Kompiling ", show (length tests), " definitions." ]
    results <- liftIO $ parallel $ zipWith (mkKompileProc (length tests)) [1 ..] tests
    return (map snd results)
  where
    mkKompileProc :: Int ->  Int -> TestCase -> IO (Bool, TestCase)
    mkKompileProc totalTestN pnum tc = do
      let args = [definition tc]
      printInfoMsg "kompile" args pnum totalTestN
      (_, _, Just stderr, phandle) <- createProcess (proc "kompile" args){std_err=CreatePipe}
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure _ -> do
          errMsg <- hGetContents stderr
          putStrLn $ concat [ "FAILURE: program ", show pnum, " failed with error:\n    ", errMsg ]
          return (False, tc)
        ExitSuccess -> return (True, tc)

runPdfs :: Bool -> Int -> [TestCase] -> K ()
runPdfs verbose _ tests = liftIO $ do
    putStrLn $ concat [ "Generating PDFs for ", show (length tests), " definitions." ]
    parallel_ $ zipWith (mkPdfProc (length tests)) [1 ..] tests
  where
    mkPdfProc :: Int -> Int -> TestCase -> IO Bool
    mkPdfProc totalTestN pnum tc = do
      let args = [definition tc, "--backend=pdf"]
      printInfoMsg "kompile" args pnum totalTestN
      (_, _, Just stderr, phandle) <- createProcess (proc "kompile" args){std_err=CreatePipe}
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure _ -> do
          errMsg <- hGetContents stderr
          putStrLn $ concat [ "FAILURE: program ", show pnum, " failed with error:\n    ", errMsg ]
          return False
        ExitSuccess -> return True

runKRuns :: Bool -> Int -> Int -> [TestCase] -> K ()
runKRuns verbose _ timeout tests = do
    ts <- liftM concat $ mapM collectCompFiles tests
    liftIO . putStrLn $ concat
      [ "Running ", show (length ts), " programs in ", show (length tests), " test cases.\n"
      , "    ", show $ countJusts (\(_, _, stdin, _, _) -> stdin) ts, " with inputs, "
      , show $ countJusts (\(_, _, _, stdout, _) -> stdout) ts, " with outputs, "
      , show $ countJusts (\(_, _, _, _, stderr) -> stderr) ts, " with errors."
      ]
    rets <- liftIO $ parallel $ zipWith (mkKrunProc (length ts)) [1 ..] ts
    liftIO $ print rets
    liftIO . putStrLn $ if and rets then "SUCCESS" else "FAILED"
  where
    -- | Run program, passing stdin file as input,
    --     - if it returns 0, compare output with stdout file (if it exists)
    --     - if it returns non-zero, compare stderr with stderr file (same)
    -- TODO: what happens to stdout if program returned non-zero?
    mkKrunProc :: Int -> Int -> (FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath) -> IO Bool
    mkKrunProc totalTestN pnum (defPath, prog, stdinf, stdoutf, stderrf) = do
      let args = [prog, "--directory=" ++ takeDirectory defPath]
      printInfoMsg "krun" args pnum totalTestN
      (Just stdin, Just stdout, Just stderr, phandle) <-
        createProcess (proc "krun" args){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe}
      maybe (return ()) (\f -> hPutStr stdin =<< readFile f) stdinf
      exitCode <- waitForProcess phandle
      case exitCode of
        ExitFailure _ -> do
          err <- hGetContents stderr
          case stderrf of
            Nothing -> do
              putStrLn $ concat [ "FAILURE: program ", show pnum
                                , " failed with error. (test doesn't have error file)" ]
              return False
            Just f -> do
              errorFile <- readFile f
              if errorFile == err
                then return True
                else do
                  putStrLn $ concat [ "FAILURE: program ", show pnum
                                    , " failed with error. (error outputs don't match)" ]
                  return False
        ExitSuccess ->
          case stdoutf of
            Nothing -> return True
            Just f -> do
              outputFile <- readFile f
              output <- hGetContents stdout
              if outputFile == output
                then return True
                else do
                  putStrLn $ concat [ "FALURE: program ", show pnum, " failed. (outputs don't match)" ]
                  return False

    checkExt :: String -> FilePath -> Bool
    --checkExt ext path | trace (show (ext, path)) False = undefined
    checkExt ext path = case takeExtension path of
                          '.' : rest -> ext == rest
                          _ -> False

    -- | Collect (definition path, program, stdin, stdout, stderr) file paths for test case,
    --   all paths should be absolute
    collectCompFiles :: TestCase -> K [(FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)]
    collectCompFiles tc@TestCase{definition=defPath, excludes=excludes, programs=programs, progFileExtension=ext} =
      case ext of
        Nothing   -> -- skip the test
                     return []
        Just ext' -> liftIO $
          liftM concat $ forM programs $ \p -> do
            dirContents <- getDirectoryContents p
            let progFiles = filter (\f -> checkExt ext' f && (takeFileName f `notElem` excludes)) dirContents
            return $ map (\f ->
                         let stdinf  = dropExtension f `addExtension` ext' `addExtension` "in"
                             stdoutf = dropExtension f `addExtension` ext' `addExtension` "out"
                             stderrf = dropExtension f `addExtension` ext' `addExtension` "err"

                             stdinr  = if stdinf  `elem` dirContents then Just (p </> stdinf) else Nothing
                             stdoutr = if stdoutf `elem` dirContents then Just (p </> stdoutf) else Nothing
                             stderrr = if stderrf `elem` dirContents then Just (p </> stderrf) else Nothing
                          in (defPath, p </> f, stdinr, stdoutr, stderrr)) progFiles

    countJusts :: (a -> Maybe b) -> [a] -> Int
    countJusts fn []       = 0
    countJusts fn (x : xs) = maybe 0 (const 1) (fn x) + countJusts fn xs

printInfoMsg :: String -> [String] -> Int -> Int -> IO ()
printInfoMsg pname args pnum total =
    putStrLn $ concat [ pname, " ", unwords args, " [", show pnum, " / ", show total, "]" ]

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
