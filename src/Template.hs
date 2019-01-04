module Template
  ( TemplateDesc(..)
  , get
  , Template
  , emptyTemplate
  , variables
  , substitute
  , render
  ) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (nub)
import Text.Microstache.Parser (parseMustache)
import Text.Microstache.Type (Node(..), Key(..))

import qualified Errors (parseMustache, ioError)
import Variables.Types (Variable, Variables, find)

data TemplateDesc = TemplateDesc
  { engine :: String
  , path :: FilePath
  }

type Template = [Node]

emptyTemplate :: Template
emptyTemplate = []

get :: TemplateDesc -> IO Template
get TemplateDesc{ engine=_, path=p } = do
  c <- catch (readFile p) Errors.ioError
  case parseMustache p (TL.pack c) of
    Left e -> Errors.parseMustache e
    Right ns -> return ns

variables :: Template -> [String]
variables = nub . concatMap getKey . filter isVar
  where
   isVar (EscapedVar _) = True
   isVar (UnescapedVar _) = True
   isVar n = False
   getKey (EscapedVar k) = T.unpack <$> unKey k
   getKey (UnescapedVar k) = T.unpack <$> unKey k
   getKey _ = [""]

substitute :: Template -> Variables -> Template
substitute [] _ = []
substitute (EscapedVar v :ts) vs = substituted : substitute ts vs
  where
    substituted = maybe (EscapedVar v) TextBlock $ find vs (keyToString v)
substitute (UnescapedVar v :ts) vs = substituted : substitute ts vs
  where
    substituted = maybe (UnescapedVar v) TextBlock $ find vs (keyToString v)
substitute (t:ts) vs = t : substitute ts vs

keyToString :: Key -> String
keyToString = T.unpack . head . unKey

render :: Template -> Maybe String
render [] = Just ""
render (TextBlock t :ts) = (Just . T.unpack $ t) <> render ts
render _ = Nothing

