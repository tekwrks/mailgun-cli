module Config
  ( decodeContext
  , Context(..)
  , getVariables
  , Variable
  , Variables
  ) where

import System.IO
import Data.Either
import Data.Text (unpack)
import Data.HashMap.Strict (toList)
import Mail.Hailgun
import Data.Yaml
import Data.Aeson.Types (typeMismatch)

import Context.Types (Context(..))
import Variables.Types (Variable, Variables)

data Template = Template
  { engine :: String
  , path :: Maybe FilePath
  }

data Config = Config
  { domain :: Maybe String
  , apiKey :: Maybe String
  , variables :: Maybe Object
  , template :: Template
  }

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .:? "domain"
    <*> v .:? "apiKey"
    <*> v .:? "variables"
    <*> (v .: "template" >>= parseTemplate)
      where
        parseTemplate (Object t) = Template
          <$> t .:? "engine" .!= "mustache"
          <*> t .: "path"
  parseJSON invalid = typeMismatch "Config" invalid

configToContext :: Config -> Context
configToContext c = Context (Config.domain c) (Config.apiKey c)

decodeContext :: FilePath -> IO (Either ParseException Context)
decodeContext fp = do
  parsed <- decodeFileEither fp
  return $ configToContext <$> parsed

parseVariables :: Maybe Object -> Variables
parseVariables Nothing = []
parseVariables (Just o) = fmap parseVariable l
  where
    l = toList o
    parseVariable v = (unpack . fst $ v, extract . snd $ v)
    extract (String t) = unpack t
    extract (Number s) = show s
    extract Null = ""
    extract _ = ""

getVariables :: FilePath -> IO (Either ParseException Variables)
getVariables fp = do
  parsed <- decodeFileEither fp
  return $ either invalid valid parsed
  where
    invalid = Left
    valid = Right . parseVariables . variables

