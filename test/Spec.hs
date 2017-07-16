import qualified UnificationTest
import qualified LambdaTypingTest
import qualified LambdaParseTest

main :: IO ()
main = do
  UnificationTest.main
  LambdaTypingTest.main
  LambdaParseTest.main
