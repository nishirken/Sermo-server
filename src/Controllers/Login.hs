module Controllers.Login where

import Web.Scotty (html, ActionM)
import Lucid (renderText)
import Views.LoginForm

loginController :: ActionM ()
loginController =
    html . renderText $ loginFormView
