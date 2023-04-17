module Applicative ( applic
                   , applicFunc
                   , multiplyMaybe
                   , tupleApplicative
                   , prodApplicative
                   , fmapApplyTuple
                   , liftA2Tuple
                   , plusApp
                   , plusLift
                   ) where

import Data.Monoid
import Control.Applicative (liftA2)

-- Applicatives are monoidal functors. They allow function application lifted over structure, but the function
-- is also embedded in some kind of structure. Every applicative must also be a functor.

{- instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x -}

applic :: [Integer]
applic = pure (+1) <*> [1..3]

applicFunc :: [Int]
applicFunc = (+1) <$> [1..3] :: [Int]

-- result is Nothing
multiplyMaybe :: Maybe Integer
multiplyMaybe = Just (*3) <*> Nothing

-- While the two-tuple functor ignores the first value, Applicative does not. This returns ("Wo Hoo!", 1)
-- We don't need a Monoid instance for the second value because function application will produce the b
tupleApplicative :: (String, Int)
tupleApplicative = ("Woo", (+1)) <*> (" Hoo!", 0 :: Int)

prodApplicative :: (Product Int, Int)
prodApplicative = (Product (3 :: Int), (+9)) <*>  (Product 2, 8 :: Int)

-- This applies the tuple constructor to the first list -> [(1, ), (2, )], then uses <*> as normal
-- [(1, 3), (1, 4), (2, 3), (2, 4)]
fmapApplyTuple :: [(Int, Int)]
fmapApplyTuple = (,) <$> [1, 2] <*> [3, 4]

-- the above can also be done with liftA2
liftA2Tuple :: [(Int, Int)]
liftA2Tuple = liftA2 (,) [1, 2] [3, 4]

-- other equal examples
plusApp :: [Int]
plusApp = (+) <$> [1, 2] <*> [3, 4]

plusLift :: [Int]
plusLift = liftA2 (+) [1, 2] [3, 4]