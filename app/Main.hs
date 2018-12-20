module Main where

import Environment (get, Environment(..))
import qualified Template (substitute, variables)

main :: IO ()
main = do
  env <- get
  -- print env
  let substituted = Template.substitute (template env) (variables env)
  print substituted
  print $ Template.variables substituted
  -- econtext <- Context.create env
  -- case econtext of
  --   Left err -> do
  --     print err
  --     exitWith $ ExitFailure 1
  --   Right context -> print context
  -- template <- Builder.getTemplate env
  -- print template

