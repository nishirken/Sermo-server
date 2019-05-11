module Models.DbUser where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.FromRow as PSQL
import qualified Database.PostgreSQL.Simple.Types as PSQLTypes

data DbUser = DbUser
  { _id :: Int
  , _email :: Text.Text
  , _friendsIds :: [Int]
  } deriving (Eq, Show)

instance PSQL.FromRow DbUser where
  fromRow = DbUser <$> PSQL.field <*> PSQL.field <*> (PSQLTypes.fromPGArray <$> PSQL.field)
