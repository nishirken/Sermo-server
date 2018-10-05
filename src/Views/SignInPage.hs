{-# LANGUAGE OverloadedStrings #-}

module Views.SignInPage where

import Lucid
import Views.BaseHtml
import Views.SwitchButtons
import Views.ErrorTitle
import Models

signInFormView :: Monad m => HtmlT m ()
signInFormView = form_ [action_ "/SignIn", method_ "post"] $ do
    input_ [type_ "email" , name_ "email", required_ "true", placeholder_ "email"]
    input_ [type_ "password", name_ "password", required_ "true", placeholder_ "password"]
    input_ [type_ "password", name_ "repeatedPassword", required_ "true", placeholder_ "repeat password"]
    button_ [type_ "submit"] "Sign in"

signInPageView :: Monad m => FormPageView -> HtmlT m ()
signInPageView (FormPageView error) = baseHtml $ do
    switchButtonsView False
    signInFormView
    errorTitle error

