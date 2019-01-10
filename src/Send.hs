module Send
  ( send
  ) where

import Mail.Hailgun
import Data.ByteString.Char8 (pack)
import qualified Errors (strError)
import qualified Data.Text as T

import Header (Header(..))
import Message (RenderedMessage(..))

send :: HailgunContext -> RenderedMessage -> Header -> IO (Either HailgunErrorResponse HailgunSendResponse)
send context content header = do
  msg <- wrapMessage (wrapContent content) header
  sendEmail context msg

wrapContent :: RenderedMessage -> MessageContent
wrapContent RenderedMessage{ plainR=plain, htmlR=(Just html) } =
  TextAndHTML (pack plain) (pack html)
wrapContent RenderedMessage{ plainR=plain, htmlR=Nothing } =
  TextOnly $ pack plain

wrapMessage :: MessageContent -> Header -> IO HailgunMessage
wrapMessage content header =
  case hailgunMessage
          (T.pack . subject $ header)
          content
          (pack . from $ header)
          (MessageRecipients
            (pack <$> to header)
            (pack <$> cc header)
            (pack <$> bcc header)
          )
          []
  of
    Left e -> Errors.strError e
    Right msg -> return msg

