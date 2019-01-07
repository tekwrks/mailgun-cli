module Message
  ( Message(..)
  , RenderedMessage(..)
  , render
  , substitute
  ) where

import Variables.Types (Variables)
import Template (Template(..))
import qualified Template (substitute, variables, render)
import qualified Errors (couldNotRender, noValueForVariables)

data Message = Message
  { plain :: Template
  , html :: Maybe Template
  } deriving (Show)

data RenderedMessage = RenderedMessage
  { plainR :: String
  , htmlR :: Maybe String
  } deriving (Show)

render :: Message -> IO RenderedMessage
render Message{ plain=p, html=Nothing } = do
  rp <- renderTemplate p
  return $ RenderedMessage rp Nothing
render Message{ plain=p, html=(Just h) } = do
  rp <- renderTemplate p
  rh <- renderTemplate h
  return $ RenderedMessage rp (Just rh)

renderTemplate :: Template -> IO String
renderTemplate t = do
  let emptys = Template.variables t
  if null emptys
     then maybe Errors.couldNotRender return $ Template.render t
     else Errors.noValueForVariables emptys

substitute :: Message -> Variables -> Message
substitute Message{ plain=p, html=Nothing } vs =
  Message (Template.substitute p vs) Nothing
substitute Message{ plain=p, html=(Just h) } vs =
  Message (Template.substitute p vs) (Just $ Template.substitute h vs)

