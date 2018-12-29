module RestHandlers.Utils where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Web.Scotty (ActionM, status, param, rescue, json)
import Network.HTTP.Types (mkStatus, status500)
import RestHandlers.Types (SuccessResponse (..))

getParam :: T.Text -> ActionM T.Text
getParam paramName =
    param (fromStrict paramName) `rescue` \errorMessage -> return $ toStrict errorMessage

makeStatus :: Int -> T.Text -> ActionM ()
makeStatus code message =
    status $ mkStatus code $ encodeUtf8 message

internalErrorStatus :: ActionM ()
internalErrorStatus = status status500

successResponse :: ActionM ()
successResponse = json $ SuccessResponse True
