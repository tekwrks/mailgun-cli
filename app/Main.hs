module Main where

import Environment (get, Environment(..))
import qualified Message (render, substitute)
import qualified Send (send)

main :: IO ()
main = do
  env <- get
  print env
  content <- Message.render $
    Message.substitute (message env) (variables env)
  res <- Send.send (context env) content (header env)
  print res

