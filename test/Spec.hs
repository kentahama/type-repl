import qualified UnificationTest
import qualified LambdaTypingTest

main :: IO ()
main = do
  UnificationTest.main
  LambdaTypingTest.main
