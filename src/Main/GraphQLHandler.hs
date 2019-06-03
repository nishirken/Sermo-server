{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main.GraphQLHandler where

import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import qualified Rest.Auth as Auth
import qualified GraphQL
import Control.Monad.IO.Class (liftIO)
import Models.GraphQLErrorResponse (GraphQLErrorResponse (..), GraphQLError (..))
import Models.GraphQLRequest (GraphQLRequest (..))

type GraphqlResponseHandler = PSQL.Connection -> Text.Text -> IO GraphQL.Response

graphqlHandler :: Text.Text -> PSQL.Connection -> GraphqlResponseHandler -> Scotty.ActionM ()
graphqlHandler authKey dbConn handler = do
  GraphQLRequest {..} <- Scotty.jsonData :: Scotty.ActionM GraphQLRequest
  isAuthorized <- (liftIO $ Auth.isTokenValid authKey _token dbConn)
  response <- (if isAuthorized == True
    then (liftIO $ handler dbConn _body)
    else pure $ GraphQLErrorResponse [GraphQLError "Not authorized"])
  Scotty.json response
