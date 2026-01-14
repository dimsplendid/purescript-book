module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number as Math
import Data.Picture (Shape(..), origin)

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

circleAtOrigin :: Shape
circleAtOrigin = Circle {x: 0.0, y: 0.0} 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (r * 2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (2.0 * w) (2.0 * h)
doubleScaleAndCenter (Line {x: x1, y: y1} {x: x2, y: y2}) = Line {x: x1 - x2, y: y1 - y2} {x: x2 - x1, y: y2 - y1}
doubleScaleAndCenter (Text _ text) = Text origin text

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = Watt (i * v)

area :: Shape -> Number
area (Circle _ r) = r * r * Math.pi
area (Rectangle _ w h) = w * h
area _ = 0.0