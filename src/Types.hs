{-# OPTIONS_GHC -Wall #-}

module Types where


type KompileOption = (String, String)   -- ^ (name, value) pair

data PgmOption
    = AllPgms [(String, String)]        -- ^ krun options for all programs
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
