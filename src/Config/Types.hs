module Config.Types where

import Variables.Types (Variables)
import Template (TemplateDesc)

data Config = Config
  { domain :: Maybe String
  , apiKey :: Maybe String
  , variables :: Variables
  , template :: Maybe TemplateDesc
  , flags :: Maybe [String]
  }

