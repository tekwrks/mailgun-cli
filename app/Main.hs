module Main where

import qualified Errors (couldNotRender, noValueForVariables)
import Environment (get, Environment(..))
import Template (Template)
import qualified Template (substitute, variables, render)

main :: IO ()
main = do
  env <- get
  print env
  -- let substituted = Template.substitute (template env) (variables env)
  -- print substituted
  -- rendered <- render substituted
  -- print rendered

render :: Template -> IO String
render t = do
  let emptys = Template.variables t
  if null emptys
     then maybe Errors.couldNotRender return $ Template.render t
     else Errors.noValueForVariables emptys

