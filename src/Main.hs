{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where


import           Control.Monad.Error    (runErrorT, throwError, strMsg)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Reader
import           Options.Applicative
import           System.Directory       (getDirectoryContents)
import           System.Exit            (exitFailure, exitSuccess)
import           System.FilePath        (takeExtension)

import           CmdArgs
import           Types


run :: K ()
run = undefined

-- TODO: show help message when it's run without arguments
main :: IO ()
main = do
    args <- execParser opts
    ret <- runErrorT (runReaderT (runK run) (validate args))
    case ret of
      Left err -> print err >> exitFailure
      Right () -> exitSuccess
  where
    opts = info (helper <*> argParser)
      ( fullDesc
     -- <> progDesc "ktest"
     -- <> header ""
      )
