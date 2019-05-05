import Test.Hspec (hspec)
import Rest.LoginSpec (loginSpec)
import Rest.SigninSpec (signinSpecApi, signinSpecDb)
import Rest.AuthSpec (authSpecIO)
import ModelsSpec (modelsSpec)
import DbSpec (dbSpec)

main :: IO ()
main = hspec $ do
  loginSpec
  signinSpecApi
  signinSpecDb
  authSpecIO
  modelsSpec
  dbSpec
