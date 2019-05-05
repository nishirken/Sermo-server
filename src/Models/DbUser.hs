module Models.DbUser where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.FromRow as PSQL

data DbUser = DbUser
  { _id :: Int
  , _email :: Text.Text
  , _friendsIds :: [Int]
  }

instance PSQL.FromRow DbUser where
  fromRow = DbUser <$> PSQL.field <*> PSQL.field <*> PSQL.fromRow
