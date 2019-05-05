{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=))
import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified GraphQL
import Models (JSONError (..), JSONResponse (..), SuccessResponse (..), GraphQLRequest (..))
import Control.Monad.IO.Class (liftIO)

makeDataResponse :: Yaml.ToJSON a => a -> Scotty.ActionM ()
makeDataResponse x = Scotty.json . Yaml.toJSON $ JSONResponse (Just x) Nothing

makeErrorResponse :: Int -> Maybe Text.Text -> Scotty.ActionM ()
makeErrorResponse code message =
  Scotty.json . Yaml.toJSON $ JSONResponse (Nothing :: Maybe Int) $ Just (JSONError code message)

makeSuccessResponse :: Scotty.ActionM ()
makeSuccessResponse = makeDataResponse $ SuccessResponse True

makeInternalErrorResponse :: Scotty.ActionM ()
makeInternalErrorResponse = makeErrorResponse 500 $ Just "Internal error"

type QueryHandler = PSQL.Connection -> Text.Text -> IO GraphQL.Response

graphqlHandler :: Text.Text -> PSQL.Connection -> QueryHandler -> Scotty.ActionM ()
graphqlHandler authKey dbConn handler = do
  GraphQLRequest { _body, _token } <- Scotty.jsonData :: Scotty.ActionM GraphQLRequest
  liftIO (handler dbConn _body) >>= Scotty.json
