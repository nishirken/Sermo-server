import Test.Hspec (hspec)
import Rest.LogInSpec (logInSpec)
import Rest.SignInSpec (signInSpecIO)

main :: IO ()
main = hspec $ do
  logInSpec
  signInSpecIO
