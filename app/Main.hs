{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Server (startServer)
import Config (makeAppConfig)
import Db (makeConnection, prepareDb)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import qualified Text.Regex as Regex

myParser :: QuasiQuoter
myParser = QuasiQuoter
  { quoteExp = \input -> lift $ Regex.subRegex (Regex.mkRegex "\\$") input "44"
  , quotePat  = const $ error "skip"
  , quoteType = const $ error "skip"
  , quoteDec = const $ error "skip"
  }

main :: IO ()
main = do
  print "some"
  -- config <- makeAppConfig
  -- connection <- makeConnection config
  -- prepareDb connection
  -- startServer config connection
