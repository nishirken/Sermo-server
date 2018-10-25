{-# LANGUAGE OverloadedStrings #-}

module Db (getUserByEmail, setUser) where

import qualified Data.Text.Lazy as TLazy
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)
import Crypto.Scrypt (defaultParams, encryptPassIO, Pass (..), getEncryptedPass)

getUserByEmail :: Connection -> TLazy.Text -> IO [(Int, TLazy.Text, TLazy.Text)]
getUserByEmail dbConn email =
    query dbConn "select id, email, password from users where email = ?" (Only email)

setUser :: Connection -> TLazy.Text -> TLazy.Text -> IO [Only Int]
setUser dbConn email password = do
    hash <- encryptPassIO defaultParams (Pass . encodeUtf8 . TLazy.toStrict $ password)
    query dbConn "insert into users (email, password) values (?,?) returning id;" (email, getEncryptedPass hash)
