module Models.DbFullUserCreds where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.FromRow as PSQL

data DbFullUserCreds = DbFullUserCreds
  { _id :: Int
  , _email :: Text.Text
  , _password :: Text.Text
  }

instance PSQL.FromRow DbFullUserCreds where
  fromRow = DbFullUserCreds <$> PSQL.field <*> PSQL.field <*> PSQL.field
