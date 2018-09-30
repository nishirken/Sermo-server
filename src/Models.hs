module Models where

import Data.Text.Lazy (Text)

data Signin = Signin {
    email :: Text
    , password :: Text
    , repeatedPassword :: Text
    }

data FormPageView = FormPageView {
    error :: Text
}
