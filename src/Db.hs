{-# LANGUAGE OverloadedStrings #-}

module Db where

import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)

getUserByEmail :: Connection -> Text -> IO [(Text, Text)]
getUserByEmail dbConn email =
    query dbConn "select email, password from users where email = ?" (Only email)
