module Test.MySolutions where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n
    | n < 0     = isEven (-n)
    | otherwise = isEven (n-2)

countEven :: Array Int -> Int
countEven arr = case uncons arr of
    Nothing -> 0
    Just {head: x, tail: xs}
        | isEven(x) -> countEven xs + 1
        | otherwise -> countEven xs