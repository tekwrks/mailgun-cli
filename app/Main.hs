module Main where

import System.Environment (getArgs)

import qualified Flags (parse)
import qualified Context (create)
import qualified Template (get, variables)

main :: IO ()
main = do
  flags <- Flags.parse =<< getArgs
  print flags
  context <- Context.create flags
  print context
  template <- Template.get "template.mustache"
  print template
  print $ Template.variables template

