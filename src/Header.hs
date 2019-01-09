module Header
  ( get
  , Header(..)
  ) where

import Control.Applicative ((<|>))

import Flags (Flags)
import qualified Flags as Flag (Flag(..))
import Config.Types (Config)
import qualified Config.Types as Config (Config(..))
import qualified Errors (noFrom, noTo, noSubject)

data Header = Header
  { from :: String
  , to :: [String]
  , cc :: [String]
  , bcc :: [String]
  , subject :: String
  } deriving (Show)

get :: Flags -> Maybe Config -> IO Header
get flags Nothing = do
  f <- maybe Errors.noFrom return $ fromFlags flags
  t <- maybe Errors.noTo return $ toFlags flags
  c <- maybe (return []) return $ ccFlags flags
  b <- maybe (return []) return $ bccFlags flags
  s <- maybe Errors.noSubject return $ subjectFlags flags
  return $ Header f t c b s
get flags (Just config) = do
  f <- maybe Errors.noFrom return $ (fromFlags flags) <|> (Config.from config)
  t <- maybe Errors.noTo return $ (toFlags flags) <|> (Config.to config)
  c <- maybe (return []) return $ (ccFlags flags) <|> (Config.cc config)
  b <- maybe (return []) return $ (bccFlags flags) <|> (Config.bcc config)
  s <- maybe Errors.noSubject return $ (subjectFlags flags) <|> (Config.subject config)
  return $ Header f t c b s

fromFlags :: Flags -> Maybe String
fromFlags [] = Nothing
fromFlags (Flag.From f :fs) = Just f
fromFlags (f:fs) = fromFlags fs

toFlags :: Flags -> Maybe [String]
toFlags [] = Nothing
toFlags (Flag.To f :fs) = Just f
toFlags (f:fs) = toFlags fs

ccFlags :: Flags -> Maybe [String]
ccFlags [] = Nothing
ccFlags (Flag.Cc f :fs) = Just f
ccFlags (f:fs) = ccFlags fs

bccFlags :: Flags -> Maybe [String]
bccFlags [] = Nothing
bccFlags (Flag.Bcc f :fs) = Just f
bccFlags (f:fs) = bccFlags fs

subjectFlags :: Flags -> Maybe String
subjectFlags [] = Nothing
subjectFlags (Flag.Subject f :fs) = Just f
subjectFlags (f:fs) = fromFlags fs

