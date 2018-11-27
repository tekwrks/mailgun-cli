module Template where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (nub)
import System.Exit
import System.IO
import Text.Microstache.Parser (parseMustache)
import Text.Microstache.Type (Node(..), Key(..))

get :: FilePath -> IO [Node]
get fp = do
  c <- catch (readFile fp) errHandler
  case parseMustache fp (TL.pack c) of
    Left e2 -> do
      hPrint stderr e2
      exitWith $ ExitFailure 1
    Right ns -> return ns
  where
    errHandler :: IOError -> IO String
    errHandler e = do
      hPrint stderr e
      exitWith $ ExitFailure 1

variables :: [Node] -> [T.Text]
variables = nub . concatMap getKey . filter isVar
  where
   isVar (EscapedVar _) = True
   -- isVar (UnescapedVar _) = True
   isVar n = False
   getKey (EscapedVar k) = unKey k
   -- getKey (UnescapedVar k) = unKey k
   getKey _ = [T.pack ""]

