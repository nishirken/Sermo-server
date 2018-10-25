module Handlers.Utils where

import qualified Data.Text.Lazy as TLazy
import Data.Text.Encoding (encodeUtf8)
import Web.Scotty (ActionM, status, param, rescue, json)
import Network.HTTP.Types (mkStatus, status500)
import Handlers.Types (SuccessResponse (..))

getParam :: TLazy.Text -> ActionM TLazy.Text
getParam paramName =
    param paramName `rescue` \errorMessage -> return errorMessage

makeStatus :: Int -> TLazy.Text -> ActionM ()
makeStatus code message =
    status $ mkStatus code $ (encodeUtf8 . TLazy.toStrict) message

internalErrorStatus :: ActionM ()
internalErrorStatus = status status500

successResponse :: ActionM ()
successResponse = json $ SuccessResponse True
