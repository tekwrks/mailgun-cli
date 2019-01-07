module Main where

import Environment (get, Environment(..))
import qualified Message (render, substitute)

main :: IO ()
main = do
  env <- get
  print env
  let substituted = Message.substitute (message env) (variables env)
  msg <- Message.render $ substituted
  print msg

