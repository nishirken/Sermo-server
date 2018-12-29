{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Db (getUserByEmail, setUser, makeConnection, prepareDb) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query, connectPostgreSQL, Query)
import Crypto.Scrypt (defaultParams, encryptPassIO, Pass (..), getEncryptedPass)
import Config (Config (..))

makeConnection :: Config -> IO Connection
makeConnection Config{ dbHost, dbPort, dbUser, dbName } =
  connectPostgreSQL $ encodeUtf8 $
    "host='" <> dbHost <> "' "
    <> "port=" <> T.pack (show dbPort) <> " "
    <> "user='" <> dbUser <> "' "
    <> "dbname='" <> dbName <> "'"

prepareDb :: Connection -> IO Connection
prepareDb conn = do
  execute
    conn
    (
      "create table if not exists users " <>
      "(id serial primary key, email varchar(50) not null, password varchar(500) not null unique);" <>
      "create table if not exists friends " <>
      "(user_id primary key, friends_ids int []);"
    )
    ()
  pure conn

getUserByEmail :: Connection -> T.Text -> IO [(Int, T.Text, T.Text)]
getUserByEmail dbConn email =
    query dbConn "select id, email, password from users where email = ?" (Only email)

setUser :: Connection -> T.Text -> T.Text -> IO [Only Int]
setUser dbConn email password = do
    hash <- encryptPassIO defaultParams (Pass . encodeUtf8 $ password)
    query dbConn "insert into users (email, password) values (?,?) returning id;" (email, getEncryptedPass hash)
