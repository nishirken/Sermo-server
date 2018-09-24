{-# LANGUAGE OverloadedStrings #-}

module Views.LoginForm where

import Lucid
import Views.BaseHtml

loginFormView :: Monad m => HtmlT m ()
loginFormView = baseHtml $
    div_ $ do
        (p_ "Login")
        form_ $
            (input_ [type_ "text"]) >>
            (input_ [type_ "text"])
