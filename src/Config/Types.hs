module Config.Types where

import Variables.Types (Variables)
import Template (TemplateDesc)

data Config = Config
  { domain :: Maybe String
  , apiKey :: Maybe String
  , flags :: Maybe [String]
  , variables :: Maybe Variables
  , subject :: Maybe String
  , from :: Maybe String
  , to :: Maybe [String]
  , cc :: Maybe [String]
  , bcc :: Maybe [String]
  , plain :: Maybe TemplateDesc
  , html :: Maybe TemplateDesc
  }

