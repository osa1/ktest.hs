{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where


import           Control.Monad.Error


type KompileOption = (String, Maybe String)   -- ^ (name, value) pair

data PgmOption
    = AllPgms [(String, Maybe String)]        -- ^ krun options for all programs
    | PgmOpt String [(String, Maybe String)]  -- ^ krun options for specific program
    deriving (Show)

data TestCase = TestCase
    { definition                 :: FilePath
    , programs                   :: [FilePath]
    , progFileExtension          :: Maybe String
    , excludes                   :: [String]
    , result                     :: Maybe FilePath
    , kompileOptions             :: [KompileOption]
    , programSpecificKRunOptions :: [PgmOption]
    } deriving (Show)

data SkipOpt = SkipKompile | SkipPdf | SkipKRun deriving (Show, Eq)

data KTestOptions = KTestOptions
    { verbose :: Bool
    , threads :: Int
    , timeout :: Int
    , tests   :: [TestCase]
    , skips   :: [SkipOpt]
    } deriving (Show)

data KTestError
    = InvalidTestFileFmtErr String
    | StrErr String
    deriving (Show)

instance Error KTestError where
    strMsg = StrErr

newtype K a = K { runK :: ErrorT KTestError IO a }
    deriving (Monad, MonadError KTestError , MonadIO)
