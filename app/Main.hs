module Main where

import System.Environment (getArgs)

import qualified Flags (parse)
import qualified Context (create)

main :: IO ()
main = do
  flags <- Flags.parse =<< getArgs
  print flags
  context <- Context.create flags
  print context

