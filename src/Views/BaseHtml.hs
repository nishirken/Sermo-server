{-# LANGUAGE OverloadedStrings #-}

module Views.BaseHtml where

import Lucid

baseHtml :: Monad m => HtmlT m () -> HtmlT m ()
baseHtml body =
    doctypehtml_ $ do
        head_ $ do
            link_ [rel_ "stylesheet", type_ "text/css", href_ "static/styles.css"]
        body_ $ body
