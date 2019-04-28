{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Yaml (FromJSON, Value (Object), parseJSON, (.:), decodeFileEither, ParseException)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (getCurrentDirectory)

data Config = Config {
  dbHost :: T.Text
  , dbPort :: Int
  , dbUser :: T.Text
  , dbName :: T.Text
  , appPort :: Int
  , authKey :: T.Text
}

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "dbHost"
    <*> v .: "dbPort"
    <*> v .: "dbUser"
    <*> v .: "dbName"
    <*> v .: "appPort"
    <*> v .: "authKey"

  parseJSON invalid = error $ "Can't parse Config from Yaml" <> show invalid

makeConfig :: String -> IO Config
makeConfig configPath = do
  directory <- getCurrentDirectory
  config <- decodeFileEither (directory <> configPath) :: IO (Either ParseException Config)
  case config of
    (Right conf) -> pure conf
    (Left configError) -> error $ show configError

makeAppConfig = makeConfig "/config.yaml"
makeTestConfig = makeConfig "/test-config.yaml"
