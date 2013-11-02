{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where


import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Identity

import CmdArgs (CmdArgs)


type KompileOption = (String, Maybe String)   -- ^ (name, value) pair

data PgmOption
    = AllPgms [(String, Maybe String)]        -- ^ krun options for all programs
    | PgmOpt String [(String, Maybe String)]  -- ^ krun options for specific program
    deriving (Show)

data TestCase = TestCase
    { -- required information in xml
      definition :: FilePath
      -- optional information
    , programs :: Maybe FilePath
    , progFileExtension :: Maybe String
    , excludes :: Maybe [String]
    , results :: Maybe FilePath
    , kompileOptions :: [KompileOption]
    , programSpecificKRunOptions :: [PgmOption]
    } deriving (Show)

type TestFile = [TestCase]

data KTestError
    = InvalidTestFileFmtErr String
    | StrErr String
    deriving (Show)

instance Error KTestError where
    strMsg = StrErr

newtype K a = K { runK :: ReaderT (CmdArgs Identity) (ErrorT KTestError IO) a }
    deriving (Monad, MonadReader (CmdArgs Identity), MonadError KTestError , MonadIO)
