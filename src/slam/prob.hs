{-# LANGUAGE GeneralizedNewtypeDeriving, InstanceSigs, DeriveFunctor, TypeOperators #-}

import Prelude hiding (product, (||))
import Control.Applicative
import Data.Function (on)
import Data.Ratio
import qualified Data.List as List

flength = fromIntegral . length

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies [] = []
frequencies xs = [(head x, length x) | x <- List.group $ List.sort xs]

-- 0..1
newtype Prob = P Rational deriving (Show, Num, Fractional, Eq, Ord)


newtype Dist a = D [(a, Prob)] deriving (Show, Functor)
value = fst
prob  = snd

type Joint x y = Dist (x, y) -- Dist (y, x)
type y :| x    = x -> Dist y
type Conditional y {- given -} x = y :| x
-- type Joint x y = Dist (x, y)

mass e (D xs) = sum [p | (x, p) <- xs, x == e]

data Die = D1 | D2 | D3 | D4 | D5 | D6 deriving (Show, Enum, Bounded)

-- * Given random Prob give a random sample from Dist.
sample:: Prob -> Dist a -> a
sample (P p) (D d) = go p d where
  go _ ((x,  _):[])             = x
  go p ((x,P q):xs) | p <= q    = x
                    | otherwise = go (p-q) xs


certainly :: a -> Dist a
certainly x = D [(x, P 1)]

impossible :: Dist a
impossible = D []

uniform :: [a] -> Dist a
uniform xs = D [(x, P p) | x <- xs] where p = 1.0 / flength xs

productL :: (x -> y -> xy) -> [x] -> [y] -> [xy]
productL f xs ys = [f x y | x <- xs, y <- ys]

productD :: (x -> y -> xy) -> Dist x -> Dist y -> Dist xy
productD f (D xs) (D ys) = D [ (f x y, p*q) | (x,p) <- xs, (y,q) <- ys]

diceL :: Int -> [[Die]]
diceL n =
  case n of
    0 -> [[]]
    _ -> productL (:) [D1 .. D6] (diceL (pred n))

diceD n =
  case n of
    0 -> certainly []
    _ -> productD (:) (uniform [D1 .. D6]) (diceD (pred n))

-- If the second event depends on the first,
-- it must be represented by a function that accepts values of the distribution
-- produced by the first event.
-- In other words, whereas the first event can be represented by a Dist a value,
-- the second event should be a function of type (a -> Dist b).
-- sigfpe: "a number attached to some data that gives a level of trust"
-- http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2007/02/21/refactoring-probability-distributions/

-- * P(Y,X) = P(Y|X)P(X) -- product rule
product :: (x -> Dist y) -> Dist x -> Dist (y, x)
product yGiven (D xs)
  = D [ ((y, x), pyx*px)
      | (x,      px  {-P(X)  -}) <- xs
      , (y,      pyx {-P(Y|X)-}) <- let D ys = yGiven x in ys]


data Bin = Blue | Red deriving (Show, Ord, Enum, Bounded, Eq)
data Fruit = Apple | Orange deriving (Show, Ord, Enum, Bounded, Eq)

fruit Red = D [(Apple, P (2 % 8)), (Orange, P (6 % 8))]
fruit Blue  = D [(Apple, P (3 % 4)), (Orange, P (1 % 4))]

bin = D [(Red, P (2 % 5)), (Blue, P (3 % 5))]

instance Applicative Dist where
  pure = certainly
  liftA2 = productD

instance Monad Dist where
  return = certainly

  (>>=) :: Dist x -> (x -> Dist y) -> Dist y
  d >>= f = let D ys = product f d in D [(y, p) | ((y, x), p) <- ys]

allFruit :: Dist Fruit
allFruit = collect $ do
  b <- bin
  fruit b

dtwice = product (\d -> certainly d) (uniform [D1 .. D6])
dall = product (\d -> uniform [D1 .. D6]) (uniform [D1 .. D6])

--classify :: Ord a => [a] -> [[a]]
classify f = List.groupBy ((==) `on` f) . List.sortBy (compare `on` f)

sumClasses :: [[(a, Prob)]] -> Dist a
sumClasses groups = D [(value (head group), sum (map prob group)) | group <- groups]

collect :: Ord a => Dist a -> Dist a
collect (D xs) = sumClasses (classify value xs)

-- p(fruit = product / marginalize

-- if i pick orange what is the prob i picked blue?

binL        = [Blue, Blue, Red, Red, Red]
fruitL Blue = [Apple, Apple, Orange, Orange, Orange, Orange, Orange, Orange]
fruitL Red  = [Apple, Apple, Apple, Orange]

allFruitL = frequencies $ do
  b <- binL
  fruitL b

--data y :| x = y :| x

--f || x :: (x -> Dist y) -> x -> 
--f || x = f x :| x

inferL = frequencies $ do
  b <- binL
  f <- fruitL b
  return (f, b)
  
-- allFruit , p(orange) = 9%20 = 0.45

joint = do
  b <- bin
  f <- fruit b
  case (f, b) of
    (Orange, Blue) -> return True
    _ -> return False

joint' = product fruit bin

{-
*Main> collect joint
D [(False,P (17 % 20)),(True,P (3 % 20))]
*Main> (3%20) / 0.45
1 % 3
-}

bayes :: (Eq x, Eq y, Ord y) => Dist (y, x) -> x -> y -> Prob
bayes joint x y = mass (y, x) joint / mass y (sum_ joint)

--bayes :: x :| y -> Dist y -> Dist x -> y :| x 
--bayes likelihood priors evidence = do
--  x  <- evidence
--  y  <- priors
--  x' <- likelihood y
--  return (x' / x)

infer1 = do
  b <- bin
  case b of
    Blue -> do
            f <- fruit b
            case f of Orange -> return True; _ -> return False
    _ -> return False

-- * P(Y) = sum_X P(Y,X) -- sum rule
sum_ :: Ord y => Dist (y,x) -> Dist y
sum_ (D joint) = fmap fst (sumClasses (classify (fst . value) joint))

-- marginalize 
