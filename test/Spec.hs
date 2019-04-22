import Test.Hspec (hspec)
import Rest.LogInSpec (logInSpec)
import Rest.SignInSpec (signInSpecIO)
import Rest.AuthSpec (authSpecIO)

main :: IO ()
main = hspec $ do
  -- logInSpec
  -- signInSpecIO
  authSpecIO
