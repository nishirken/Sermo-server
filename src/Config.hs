{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text.Lazy (Text, toStrict, pack)
import Data.Yaml (FromJSON, Value (Object), parseJSON, (.:), decodeFileEither, ParseException)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (getCurrentDirectory)

data Config = Config {
    dbHost :: Text
    , dbPort :: Int
    , dbUser :: Text
    , dbName :: Text
    , appPort :: Int
    , authKey :: Text
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

makeConfig :: IO (Either ParseException Config)
makeConfig = do
  directory <- getCurrentDirectory
  decodeFileEither (directory <> "/config.yaml") :: IO (Either ParseException Config)
