import Test.Hspec (hspec)
import Rest.LogInSpec (logInSpec)
import Rest.SignInSpec (signInSpecApi, signInSpecDb)
import Rest.AuthSpec (authSpecIO)

main :: IO ()
main = hspec $ do
  logInSpec
  signInSpecApi
  signInSpecDb
  authSpecIO
