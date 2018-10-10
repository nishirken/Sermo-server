{-# LANGUAGE OverloadedStrings #-}

module Db (getUserByEmail, setUser) where

import Control.Monad.Except (ExceptT(..))
import Data.Maybe (maybe)
import Data.Text.Lazy (Text, toStrict)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)
import Crypto.Scrypt (defaultParams, encryptPassIO, Pass (..), getEncryptedPass)

getUserByEmail :: Connection -> Text -> IO [(Int, Text, Text)]
getUserByEmail dbConn email =
    query dbConn "select id, email, password from users where email = ?" (Only email)

key = encodeUtf8 "passwordKey"

setUser :: Connection -> Text -> Text -> IO Int
setUser dbConn email password = do
    hash <- encryptPassIO defaultParams (Pass . encodeUtf8 . toStrict $ password)
    fromIntegral <$> execute dbConn "insert into users (email, password) values (?,?)" (email, getEncryptedPass hash)
