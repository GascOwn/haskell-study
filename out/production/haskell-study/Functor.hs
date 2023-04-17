module Functor ( replaceWithP
               , replacedMaybe
               , doubleLift
               , tripleLift
               , liftedInc
               , liftedShow
               ) where

-- Functors must abide the identity law fmap id a == id a
data Lol a =
    Asd
  | Rofl a
  | Lmao
  deriving (Eq, Show)

instance Functor Lol where
  fmap _ Asd = Asd
  fmap f (Rofl a) = Rofl (f a)
  fmap _ Lmao = Lmao

-- Functors must also about the law of composition fmap (f . g) == fmap f . fmap g
-- Everything but the last type argument must be considered part of the structure and left untouched
data CountingGood a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- Functors apply function lifting over a structure. The structure can also be something like Maybe, Either, etc
replaceWithP :: a -> Char
replaceWithP = const 'p'

values :: [Maybe String]
values = [Just "Ave", Nothing, Just "whohoo"]

-- Each composition of fmap goes one layer deeper in the structure:

-- this returns "ppp"
replacedMaybe :: [Char]
replacedMaybe = replaceWithP <$> values

-- this returns [Just 'p', Nothing, Just 'p']
doubleLift :: [Maybe Char]
doubleLift = (fmap . fmap) replaceWithP values

-- this returns [Just "ppp", Nothing, Just "pppppp"]
tripleLift :: [Maybe String]
tripleLift = (fmap . fmap . fmap) replaceWithP values

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
