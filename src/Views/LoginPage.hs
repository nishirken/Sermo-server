{-# LANGUAGE OverloadedStrings #-}

module Views.LoginPage where

import Lucid
import Views.BaseHtml
import Views.Forms
import Data.Text (pack)

linkClass x = if x then "enabled" else "disabled"

loginPageView :: Monad m => Bool -> HtmlT m ()
loginPageView isLogin = baseHtml $
    div_ [class_ "form-page"] $ do
        a_ [href_ "/login", class_ $ linkClass isLogin] "Log in"
        a_ [href_ "/signin", class_ $ linkClass $ not isLogin] "Sign in"
        if isLogin then loginFormView else signinFormView
