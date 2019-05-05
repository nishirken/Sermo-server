{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main.Schemas where

import GraphQL (interpretAnonymousQuery)
import GraphQL.API (Argument, Object, Field, (:>), List)
import GraphQL.Resolver (Handler, (:<>) (..))
import qualified Utils
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as Text
import qualified Db
import qualified Models.DbUser as DbUser

type Friend = Object "Friend" '[]
  '[ Field "email" Text.Text ]

type User = Object "User" '[]
  '[ Field "id" Int :> Field "email" Text.Text ]

type UserQuery = Object "UserQuery" '[]
  '[ Field "user" Text.Text ]

userQueryHandler :: PSQL.Connection -> Handler IO UserQuery
userQueryHandler dbConn = (pure . pure) "USER" 
    where
      userHandler id email = pure $ pure id :<> pure email
      friendsEmails = map (\DbUser.DbUser {..} -> _email)

interpretUserQuery :: Utils.QueryHandler
interpretUserQuery dbConn = interpretAnonymousQuery @UserQuery $ userQueryHandler dbConn
