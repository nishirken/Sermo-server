module Models where

import Data.Text.Lazy (Text)

data SignIn = SignIn {
    email :: Text
    , password :: Text
    , repeatedPassword :: Text
    }

data FormPageView = FormPageView {
    error :: Text
}
