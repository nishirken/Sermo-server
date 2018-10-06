module Controllers.Utils where

import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, param, rescue)

getParam :: Text -> ActionM Text
getParam paramName = do
    param paramName `rescue`
        \errorMessage -> return errorMessage
