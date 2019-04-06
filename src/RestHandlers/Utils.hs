{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RestHandlers.Utils where

import RestHandlers.Types (JSONResponse (..), JSONError (..), SuccessResponse (..))
import qualified Data.Yaml as Yaml
import Data.Yaml ((.=))
import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
    
instance Yaml.ToJSON JSONError where
    toJSON JSONError { code, message } = Yaml.object
        [ "code" .= Yaml.toJSON code
        , "message" .= Yaml.toJSON message
        ]

instance Yaml.ToJSON a => Yaml.ToJSON (JSONResponse a) where
    toJSON JSONResponse { _data, _error } = Yaml.object
        [ "data" .= Yaml.toJSON _data
        , "error" .= Yaml.toJSON _error
        ]

instance Yaml.ToJSON SuccessResponse where
    toJSON x = Yaml.object [ "success" .= Yaml.toJSON x ]

makeDataResponse :: Yaml.ToJSON a => a -> Scotty.ActionM ()
makeDataResponse x = Scotty.json . Yaml.toJSON $ JSONResponse (Just x) Nothing

makeErrorResponse :: Int -> Maybe Text.Text -> Scotty.ActionM ()
makeErrorResponse code message =
    Scotty.json . Yaml.toJSON $ JSONResponse (Nothing :: Maybe Int) $ Just (JSONError code message)

makeSuccessResponse :: Scotty.ActionM ()
makeSuccessResponse = makeDataResponse $ SuccessResponse True

makeInternalErrorResponse :: Scotty.ActionM ()
makeInternalErrorResponse = makeErrorResponse 500 $ Just "Internal error"
