{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main.Schemas where

import GraphQL (interpretAnonymousQuery)
import GraphQL.API (Argument, Object, Field, (:>), List)
import GraphQL.Resolver (Handler, (:<>) (..), ResolverError (..), Result (..))
import GraphQL.Value.ToValue (toValue)
import Data.Int (Int32)
import Main.GraphQLHandler (GraphqlResponseHandler)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as Text
import qualified Db
import qualified Models.DbUser as DbUser

type Friend = Object "Friend" '[]
  '[ Field "id" Int32, Field "email" Text.Text ]

type User = Object "User" '[]
  '[ Field "id" Int32, Field "email" Text.Text, Field "friends" (List Text.Text) ]

type RootQuery = Object "RootQuery" '[]
  '[ Argument "id" Int32 :> Field "user" (Maybe User) ]

friendHandler :: DbUser.DbUser -> Handler IO Text.Text
friendHandler DbUser.DbUser {..} = pure _email

friendsHandler :: [DbUser.DbUser] -> Handler IO (List Text.Text)
friendsHandler friends = pure $ map friendHandler friends

userHandler :: Int32 -> Text.Text -> [DbUser.DbUser] -> Handler IO User
userHandler id email friends = pure (pure id :<> pure email :<> friendsHandler friends)

rootQueryHandler :: PSQL.Connection -> Handler IO RootQuery
rootQueryHandler dbConn = pure $ \userId -> do
  result <- Db.getUserById dbConn (fromIntegral userId :: Int)
  case result of
    [DbUser.DbUser {..}] -> do
      friends <- Db.getUsersByIds dbConn _friendsIds
      pure $ Just $ userHandler (fromIntegral _id :: Int32) _email friends
    _ -> pure Nothing

interpretRootQuery :: GraphqlResponseHandler
interpretRootQuery dbConn = interpretAnonymousQuery @RootQuery $ rootQueryHandler dbConn
