module Exercises.Functor where

import GHC.Arr

data Bool' a = False' a | True' a
data Perhaps a = Nah | Some a
newtype Mu f = InF {out :: f (Mu f)}
data D = D (Array Word Word) Int Int

instance Functor Bool' where 
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)
  
instance Functor Perhaps where 
  fmap _ Nah = Nah
  fmap f (Some a) = Some (f a)
  
