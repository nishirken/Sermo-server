{-# LANGUAGE OverloadedStrings #-}
module Handlers.Types where

import qualified Web.Scotty as Scotty
import qualified Network.HTTP.Types as NetworkTypes
import qualified Data.Yaml as Yaml
import Data.Yaml ((.=))

class MapError a where
    errorToStatus :: a -> Scotty.ActionM ()

newtype SuccessResponse = SuccessResponse { success :: Bool }

instance Yaml.ToJSON SuccessResponse where
    toJSON (SuccessResponse success) = Yaml.object ["success" .= success]
