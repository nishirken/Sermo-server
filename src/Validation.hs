{-# LANGUAGE OverloadedStrings #-}

module Validation where

import qualified Data.Text.Lazy as T

passwordMatchErrorMessage = T.pack "Passwords doesn't match"

validatePasswordMatch :: T.Text -> T.Text -> Either T.Text T.Text
validatePasswordMatch pass repeated =
    if pass == repeated then Right pass else Left passwordMatchErrorMessage

minPasswordLength = 6
maxPasswordLength = 20

passwordLengthErrorMessage = T.pack $
    "Password length incorrect, min length is " ++
    (show minPasswordLength) ++
    ", max is " ++
    (show maxPasswordLength)

validatePasswordLength :: T.Text -> Either T.Text T.Text
validatePasswordLength password =
    if and [l <= maxPasswordLength, l >= minPasswordLength]
    then Right password
    else Left passwordLengthErrorMessage
        where l = T.length password

maxEmailLength = 50
emailErrorMessage = T.pack $ "Email length incorrect, max length is " ++ (show maxEmailLength)

validateEmailLength :: T.Text -> Either T.Text T.Text
validateEmailLength email =
    if T.length email <= maxEmailLength then Right email else Left emailErrorMessage
