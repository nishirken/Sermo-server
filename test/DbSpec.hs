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
  res <- liftIO (do
    conn <- dbPreparation
    Db.setUser conn testEmail "123456"
    Db.getUserCredsByEmail conn testEmail)
  it "get user by email" $ res `shouldSatisfy` (not . null)

  -- it "get user by id" (do
  --   isExists <- liftIO (do
  --     conn <- dbPreparation
  --     [(Only id)] <- Db.setUser conn testEmail "123456"
  --     res <- Db.getUserById conn id
  --     pure $ not (null res))
  --   isExists `shouldReturn` True)
