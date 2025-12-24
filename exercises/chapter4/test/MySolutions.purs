module Test.MySolutions where

import Prelude

-- import Data.Person (Person, Address)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial n k
    | n == k    = 1
    | k > n     = 0
    | otherwise = factorial n / factorial k / factorial (n-k)
 
pascal :: Int -> Int -> Int
pascal n k
    | n < k     = 0
    | n == k    = 1
    | k == 0    = 1
    | otherwise = pascal (n-1) k + pascal (n-1) (k-1)


sameCity :: forall (r1 :: Row Type) (r2 :: Row Type) . {address :: {city :: String | r2} | r1} -> {address :: {city :: String | r2} | r1} -> Boolean
sameCity {address: {city: x}} {address: {city: y}} = x == y 

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default
