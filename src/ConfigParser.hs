{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
module ConfigParser where


import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import qualified Data.ByteString.Lazy as L

import Types


parseTestCase :: UNode String -> TestCase
parseTestCase e@Element{eName="test", eAttributes=attrs, eChildren=children} =
    TestCase{
            definition=requiredField "definition" e
          , programs=maybe [] words (lookup "programs" attrs)
          , progFileExtension=lookup "extension" attrs
          , excludes=maybe [] words (lookup "exclude" attrs)
          , result=lookup "results" attrs
          , kompileOptions=nameValPairs "kompile-option" children
          , programSpecificKRunOptions=specifics children
          }
  where
    nameValPairs :: String -> [UNode String] -> [(String, Maybe String)]
    nameValPairs nname = catMaybes . map
      (\case
         e'@Element{eName=ename, eChildren=[], eAttributes=attrs'}
           | ename == nname -> Just (requiredField "name" e', lookup "value" attrs')
           | otherwise      -> Nothing
         _ -> Nothing)

    specifics :: [UNode String] -> [PgmOption]
    specifics (Element{eName="all-programs", eAttributes=[], eChildren=children'} : rest) =
      (AllPgms $ nameValPairs "krun-option" children') : specifics rest
    specifics (Element{eName="all-programs"} : _) =
      error "all-programs child has attributes"
    specifics (e'@Element{eName="program"} : rest) =
      let progName = requiredField "name" e'
          opts = catMaybes $ map
            (\case
               e''@Element{eName="krun-option", eChildren=[], eAttributes=attrs'} ->
                 Just (requiredField "name" e'', lookup "value" attrs')
               _ -> Nothing) children in
      (PgmOpt progName opts) : specifics rest
    specifics (_ : rest) = specifics rest
    specifics [] = []

    requiredField :: String -> UNode String -> String
    requiredField fname e'@Element{eAttributes=attrs'} =
      case lookup fname attrs' of
        Nothing  -> error $ "test element missing a required field `" ++ fname ++ "`\n" ++ show (format e')
        Just val -> val
parseTestCase e = error (show e)

parseTestFile :: [UNode String] -> [TestCase]
parseTestFile = map parseTestCase . removeTextNodes

removeTextNodes :: [UNode String] -> [UNode String]
removeTextNodes = filter p
  where
    p Element{} = True
    p Text{}    = False

parseConfigFile :: FilePath -> IO [TestCase]
parseConfigFile filepath = do
    fileContent <- L.readFile filepath
    let (xml, mErr) = parse defaultParseOptions fileContent :: (UNode String, Maybe XMLParseError)
    let ret = case xml of
                Element{eName="tests", eChildren=children} -> parseTestFile children
                _ -> error "error while parsing xml"
    case mErr of
      Nothing -> return ret
      Just err -> error $ "XML Parse failed: " ++ show err

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] -> parseConfigFile filename >>= print
      _ -> do
        hPutStrLn stderr "Usage: program <file.xml>"
        exitWith $ ExitFailure 1
