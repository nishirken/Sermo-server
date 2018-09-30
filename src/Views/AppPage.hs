{-# LANGUAGE OverloadedStrings #-}

module Views.AppPage where

import Lucid
import Views.BaseHtml

appPageView :: Monad m => HtmlT m ()
appPageView = baseHtml $
    div_ [class_ "page"] $ do
        h2_ "Application"
