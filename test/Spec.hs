import Control.SyntacticUnificationTest as U
import Data.Lambda.Simple.TypingTest as T
import Data.Lambda.Simple.ParseTest as P

main :: IO ()
main = do
  U.main
  T.main
  P.main
