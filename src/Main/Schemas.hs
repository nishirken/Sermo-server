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
import qualified Utils
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as Text
import qualified Db
import qualified Models.DbUser as DbUser

type Friend = Object "Friend" '[]
  '[ Field "id" Text.Text, Field "email" Text.Text ]

type User = Object "User" '[]
  '[ Field "id" Text.Text, Field "email" Text.Text, Field "friends" (List Text.Text) ]

type UserQuery = Object "UserQuery" '[]
  '[ Argument "id" Text.Text :> Field "user" (Maybe User) ]

friendHandler :: DbUser.DbUser -> Handler IO Text.Text
friendHandler DbUser.DbUser {..} = pure _email

friendsHandler :: [DbUser.DbUser] -> Handler IO (List Text.Text)
friendsHandler friends = pure $ map friendHandler friends

userHandler :: Int -> Text.Text -> [DbUser.DbUser] -> Handler IO User
userHandler id email friends = pure $
  pure ((Text.pack . show) id) :<> pure email :<> friendsHandler friends

userQueryHandler :: PSQL.Connection -> Handler IO UserQuery
userQueryHandler dbConn = pure $ \userId -> do
  result <- Db.getUserById dbConn (read @Int $ Text.unpack userId)
  case result of
    [DbUser.DbUser {..}] -> do
      friends <- Db.getUsersByIds dbConn _friendsIds
      pure $ Just $ userHandler _id _email friends
    _ -> pure Nothing

interpretUserQuery :: Utils.QueryHandler
interpretUserQuery dbConn = interpretAnonymousQuery @UserQuery $ userQueryHandler dbConn
