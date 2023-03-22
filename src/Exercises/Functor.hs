module Exercises.Functor where

import GHC.Arr

-- Add fmap, parentheses, composition for the expression to typecheck
-- expected [2]
a1 :: [Int]
a1 = (+1) <$> read "[1]" :: [Int]

-- expected Just ["Hi,lol","Hellolol"]
b1 :: Maybe [String]
b1 = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- expected c 1 -> - 2
c1 :: Num a => a -> a
c1 = (*2) . (\x -> x - 2)

-- expected d 0 -> "1[0,1,2,3]"
d1 :: Int -> String
d1 = ((return '1' ++) . show) . (\x -> [x, 1..3])

-- expected 3693
e1 :: IO Integer
e1 = let ioi = readIO "1" :: IO Integer
         changed = read . ("123"++) . show <$> ioi
     in (*3) <$> changed


data Bool' a = False' a | True' a --Yes
data Perhaps a = Nah | Some a --Yes
newtype Mu f = InF {out :: f (Mu f)} -- No
data D = D (Array Word Word) Int Int -- No

instance Functor Bool' where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

instance Functor Perhaps where
  fmap _ Nah = Nah
  fmap f (Some a) = Some (f a)

-- Rearrange arguments to the type constructor to make the Functor instance work

data Sum' b a = First a | Second b

instance Functor (Sum' e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company a c) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c