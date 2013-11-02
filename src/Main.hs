{-# OPTIONS_GHC -Wall #-}

module Main where


import Options.Applicative
import CmdArgs


run :: CmdArgs -> IO ()
run = undefined

-- TODO: show help message when it's run without arguments
main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> argParser)
      ( fullDesc
     -- <> progDesc "ktest"
     -- <> header ""
     )
