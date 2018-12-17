module Builder
  ( getTemplate
  ) where

import Control.Applicative
import Text.Microstache.Type (Node)

import qualified Template (get)
import Template (Template(..))
import qualified Config (getTemplate)
import Flags (Flag(Config, Mustache), Flags)
import Environment

getTemplate :: Environment -> IO (Maybe [Node])
getTemplate env = do
  let fs = flags env
  tFile <- fromFile fs
  tFlags <- fromArgs fs
  return $ tFlags <|> tFile

fromFile :: Flags -> IO (Maybe [Node])
fromFile [] = return Nothing
fromFile (Config config :fs) = do
  econtext <- Config.getTemplate config
  either fileError fileParsed econtext
    where
      fileError e = return Nothing
      fileParsed t = do
        case path t of
          Nothing -> return Nothing
          Just p -> Just <$> Template.get p
fromFile (f:fs) = fromFile fs

fromArgs :: Flags -> IO (Maybe [Node])
fromArgs [] = return Nothing
fromArgs (Mustache p :fs) = Just <$> Template.get p
fromArgs (f:fs) = fromArgs fs

