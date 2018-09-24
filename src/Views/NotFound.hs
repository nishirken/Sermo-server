{-# LANGUAGE OverloadedStrings #-}

module Views.NotFound where

import Lucid
import Views.BaseHtml

notFoundView :: Monad m => HtmlT m ()
notFoundView = baseHtml $ h2_ "Not found"
