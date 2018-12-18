module Main where

import qualified Environment (get)

main :: IO ()
main = do
  env <- Environment.get
  print env
  -- econtext <- Context.create env
  -- case econtext of
  --   Left err -> do
  --     print err
  --     exitWith $ ExitFailure 1
  --   Right context -> print context
  -- template <- Builder.getTemplate env
  -- print template

