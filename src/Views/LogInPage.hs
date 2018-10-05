{-# LANGUAGE OverloadedStrings #-}

module Views.LogInPage where

import Lucid
import Views.BaseHtml (baseHtml)
import Views.SwitchButtons (switchButtonsView)
import Views.ErrorTitle (errorTitle)
import Models

logInFormView :: Monad m => HtmlT m ()
logInFormView = form_ [action_ "/login", method_ "post"] $ do
    input_ [type_ "email", required_ "true", placeholder_ "email"]
    input_ [type_ "password", required_ "true", placeholder_ "password"]
    button_ [type_ "submit"] "Log in"

logInPageView :: Monad m => FormPageView -> HtmlT m ()
logInPageView (FormPageView error) = baseHtml $ do
    switchButtonsView True
    logInFormView
    errorTitle error
