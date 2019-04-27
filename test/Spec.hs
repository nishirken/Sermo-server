import Test.Hspec (hspec)
import Rest.LoginSpec (loginSpec)
import Rest.SigninSpec (signinSpecApi, signinSpecDb)
import Rest.AuthSpec (authSpecIO)
import ModelsSpec (modelsSpec)

main :: IO ()
main = hspec $ do
  loginSpec
  signinSpecApi
  signinSpecDb
  authSpecIO
  modelsSpec
