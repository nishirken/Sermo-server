{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.Graphql.Introspection where

import qualified Web.Scotty as Scotty
import qualified Rest.Auth as Auth
import qualified Data.Text as T
import Models.TokenObject (TokenObject (..))
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Utils

graphqlSchemaHandler :: T.Text -> PSQL.Connection -> Scotty.ActionM ()
graphqlSchemaHandler authKey conn = do
  TokenObject {..} <- Scotty.jsonData :: Scotty.ActionM TokenObject
  isValid <- liftIO $ Auth.isTokenValid authKey _token conn
  if isValid
    then Scotty.file "./src/Rest/Graphql/Introspection.graphql"
    else Utils.makeErrorResponse 401 $ Just "Unauthorized"
