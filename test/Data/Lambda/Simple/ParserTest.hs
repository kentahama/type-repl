module Data.Lambda.Simple.ParserTest where

import Data.Lambda.Simple.Parser

main = do
  print $ parse "\\x -> x"
  print $ parse "\\f -> \\x -> (f x)"
  print $ parse "\\x -> \\y  -> \\z -> ((x z) (y z))"
  print $ parse "(\\x -> x x)"
  print $ parse "\\x -> \\y -> \\z -> x y z"
