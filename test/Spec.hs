import Control.SyntacticUnificationTest as U
import Data.Lambda.SimpleTest as S
import Data.Lambda.Simple.TypingTest as T
import Data.Lambda.Simple.ParserTest as P

main :: IO ()
main = do
  U.mainTests
  S.mainTests
  T.mainTests
  P.mainTests
