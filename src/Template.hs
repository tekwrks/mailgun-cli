module Template
  ( TemplateDesc(..)
  , get
  , Template
  , variables
  ) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (nub)
import System.Exit
import System.IO
import Text.Microstache.Parser (parseMustache)
import Text.Microstache.Type (Node(..), Key(..))

data TemplateDesc = TemplateDesc
  { engine :: String
  , path :: FilePath
  }

type Template = [Node]

get :: TemplateDesc -> IO Template
get TemplateDesc{ engine=_, path=p } = do
  c <- catch (readFile p) errHandler
  case parseMustache p (TL.pack c) of
    Left e2 -> do
      hPrint stderr e2
      exitWith $ ExitFailure 1
    Right ns -> return ns
  where
    errHandler :: IOError -> IO String
    errHandler e = do
      hPrint stderr e
      exitWith $ ExitFailure 1

variables :: Template -> [T.Text]
variables = nub . concatMap getKey . filter isVar
  where
   isVar (EscapedVar _) = True
   isVar (UnescapedVar _) = True
   isVar n = False
   getKey (EscapedVar k) = unKey k
   getKey (UnescapedVar k) = unKey k
   getKey _ = [T.pack ""]

