{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blamesphemy where

import Type.Reflection
import Data.Maybe


data Any where
  Any :: forall a. TypeRep a -> a -> Any

toAny :: (Typeable a) => a -> Any
toAny v = Any typeRep v

fromAny :: forall a. (Typeable a) => Any -> Maybe a
fromAny (Any ra x)
  = do HRefl <- ra `eqTypeRep` (typeRep :: TypeRep a)
       return x

cast :: (Typeable a) => a -> Any
cast = toAny

-- The main idea is that it should not be allowed!
test = toAny (toAny True)
{-
-- My own implementation of Dynamic
data Any where
  -- this is supposed to be a hidden constructor
  MkAny :: TypeRep a -> a -> Any

toAny :: Typeable a => a -> Any
toAny v = MkAny typeRep v

fromAny :: forall d. Typeable d => Any -> Maybe d
fromAny (MkAny (ra :: TypeRep a) (x :: a))
  = do
    HRefl <- ra `eqTypeRep` (typeRep :: TypeRep d)
    return x

class Consistent a b where
  cast :: a -> b

-- Consistency is reflexive
-- instance Consistent a a where
-- cast = id

-- Consistency over Any is symmetric
instance (Typeable a, NotAny a ~ 'True) => Consistent a Any where
  cast = toAny

instance (Typeable b, NotAny b ~ 'True) => Consistent Any b where
  cast = fromJust . fromAny


-- Wrap rule
instance (Consistent c a, Consistent b d) => Consistent (a->b) (c->d) where
  cast f = g
    where
      g :: c -> d
      g x = cast @(b) @(d) (f (cast @(c) @(a) x))

-- This one needs to do additional work!
instance Consistent Any (Any -> Any) where
  cast (MkAny (ra :: TypeRep a) (x :: a))
    = case ra `eqTypeRep` TRFun (typeRep :: TypeRep Any) (typeRep :: TypeRep Any) of
        Just HRefl  -> x
        Nothing -> error "not a function"

f :: Integer -> Integer
f = (+5)

foo :: Any -> Any
foo = cast f

example1 :: Integer
example1 = (cast @(Any -> Any) @(Integer -> Integer) foo) 3

-- Important note
-- this one will not typecheck, due to ovelapping instances
-- test1 :: Any -> Any
-- test1 = cast @(Any) @(Any -> Any) undefined

{-
test2 :: Any
test2 = cast @(Any) @(Any) undefined
-}
-}
