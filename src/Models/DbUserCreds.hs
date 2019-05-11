module Models.DbUserCreds where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.FromRow as PSQL

data DbUserCreds = DbUserCreds
  { _email :: Text.Text
  , _password :: Text.Text
  }

instance PSQL.FromRow DbUserCreds where
  fromRow = DbUserCreds <$> PSQL.field <*> PSQL.field
