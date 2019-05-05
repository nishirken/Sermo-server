{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.Utils where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=))
import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import Models (JSONError (..), JSONResponse (..), SuccessResponse (..))

makeDataResponse :: Yaml.ToJSON a => a -> Scotty.ActionM ()
makeDataResponse x = Scotty.json . Yaml.toJSON $ JSONResponse (Just x) Nothing

makeErrorResponse :: Int -> Maybe Text.Text -> Scotty.ActionM ()
makeErrorResponse code message =
  Scotty.json . Yaml.toJSON $ JSONResponse (Nothing :: Maybe Int) $ Just (JSONError code message)

makeSuccessResponse :: Scotty.ActionM ()
makeSuccessResponse = makeDataResponse $ SuccessResponse True

makeInternalErrorResponse :: Scotty.ActionM ()
makeInternalErrorResponse = makeErrorResponse 500 $ Just "Internal error"
