{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main.GraphQLHandler where

import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as Text
import qualified Web.Scotty as Scotty
import qualified Rest.Auth as Auth
import qualified GraphQL
import qualified GraphQL.Internal.Output as InternalGraphQL
import qualified Data.List.NonEmpty as NonEmptyList
import Control.Monad.IO.Class (liftIO)
import Models.GraphQLRequest (GraphQLRequest (..))

type GraphqlResponseHandler = PSQL.Connection -> Text.Text -> IO GraphQL.Response

graphqlHandler :: Text.Text -> PSQL.Connection -> GraphqlResponseHandler -> Scotty.ActionM ()
graphqlHandler authKey dbConn handler = do
  GraphQLRequest {..} <- Scotty.jsonData :: Scotty.ActionM GraphQLRequest
  isAuthorized <- (liftIO $ Auth.isTokenValid authKey _token dbConn)
  response <- (if isAuthorized == True
    then (liftIO $ handler dbConn _body)
    else pure $ GraphQL.PreExecutionFailure $ NonEmptyList.fromList [InternalGraphQL.Error "Not authorized" []])
  Scotty.json response
