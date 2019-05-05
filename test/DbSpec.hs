{-# LANGUAGE OverloadedStrings #-}

module DbSpec (dbSpec) where

import Database.PostgreSQL.Simple (Connection, Only (..))
import Test.Hspec (describe, context, it, Spec, shouldSatisfy, before, runIO)
import qualified Db
import qualified Config
import Control.Monad.IO.Class (liftIO)

testEmail = "test@mail.ru"

dbPreparation :: IO Connection
dbPreparation = do
  config <- Config.makeTestConfig
  connection <- Db.makeConnection config
  Db.prepareDb connection
  Db.clearDb connection
  pure connection

dbSpec :: Spec
dbSpec = describe "DbSpec" $ do
  it "get user by email" $ do
    res <- liftIO (do
      conn <- dbPreparation
      Db.setUser conn testEmail "123456"
      Db.getUserCredsByEmail conn testEmail)
    res `shouldSatisfy` (not . null)

  it "get user by id" $ do
    res <- liftIO (do
      conn <- dbPreparation
      [(Only id)] <- Db.setUser conn testEmail "123456"
      Db.getUserById conn id)
    res `shouldSatisfy` (not . null)

  it "get users" $ do
    res <- liftIO (do
      conn <- dbPreparation
      [(Only id)] <- Db.setUser conn testEmail "123456"
      [(Only id')] <- Db.setUser conn "test1@email.ru" "123456"
      [(Only id'')] <- Db.setUser conn "test2@email.ru" "123456"
      Db.getUsers conn [id, id', id''])
    (length res) `shouldBe` 3
