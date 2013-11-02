{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import           Control.Monad.Error    (runErrorT, strMsg, throwError)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Reader
import           Data.Maybe             (fromJust)
import           Options.Applicative
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         getDirectoryContents, getCurrentDirectory)
import           System.Exit            (exitFailure, exitSuccess)
import           System.FilePath        (takeExtension)

import           CmdArgs                hiding (CmdArgs (..))
import           Types

import           Debug.Trace            (trace)


run :: KTestOptions -> K ()
run KTestOptions{..} = forM_ tests (runTest verbose threads timeout skips)

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
