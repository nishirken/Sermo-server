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

type QueryHandler = PSQL.Connection -> Text.Text -> IO GraphQL.Response

graphqlHandler :: Text.Text -> PSQL.Connection -> QueryHandler -> Scotty.ActionM ()
graphqlHandler authKey dbConn handler = do
  GraphQLRequest {..} <- Scotty.jsonData :: Scotty.ActionM GraphQLRequest
  resp <- (liftIO $ do
    isAuthorized <- Auth.isTokenValid authKey _token dbConn
    if isAuthorized == True
      then handler dbConn _body
      else GraphQLErrorResponse $ GraphQLError "Not authorized"
    )
  Scotty.json resp
