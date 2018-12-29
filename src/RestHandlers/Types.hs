{-# LANGUAGE OverloadedStrings #-}
module RestHandlers.Types where

import qualified Web.Scotty as Scotty
import qualified Network.HTTP.Types as NetworkTypes
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import Data.Yaml ((.=))

class MapError a where
    errorToStatus :: a -> Scotty.ActionM ()

newtype SuccessResponse = SuccessResponse { success :: Bool }

instance Yaml.ToJSON SuccessResponse where
    toJSON (SuccessResponse success) = Yaml.object ["success" .= success]

data TokenRequest = TokenRequest {
  token :: T.Text
  , body :: T.Text
}

instance Yaml.FromJSON TokenRequest where
  parseJSON (Yaml.Object v) = TokenRequest
    <$> v Yaml..: "token"
    <*> v Yaml..: "body"
