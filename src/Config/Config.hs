module Config.Config
  ( Config(..)
  , get
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
import Template (TemplateDesc(..))

import Config.Types (Config(..))

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .:? "domain"
    <*> v .:? "apiKey"
    <*> (v .: "variables" >>= parseVariables)
    <*> (v .: "template" >>= parseTemplate)
    <*> v .:? "flags"
      where
        parseTemplate (Object t) = TemplateDesc
          <$> t .:? "engine" .!= "mustache"
          <*> t .: "path"
        parseVariables :: Maybe Object -> Parser Variables
        parseVariables Nothing = return []
        parseVariables (Just o) = return $ fmap parseVariable $ toList o
          where
            parseVariable v = (unpack . fst $ v, extract . snd $ v)
            extract (String t) = unpack t
            extract (Number s) = show s
            extract Null = ""
            extract _ = ""
  parseJSON invalid = typeMismatch "Config" invalid

get :: FilePath -> IO (Either ParseException Config)
get = decodeFileEither

