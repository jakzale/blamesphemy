{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Blamesphemy where

import Type.Reflection
import Data.Maybe
import Test.HUnit


data Any where
  Any :: forall a. TypeRep a -> a -> Any

toAny :: (Typeable a) => a -> Any
toAny v = Any typeRep v

fromAny :: forall a. (Typeable a) => Any -> Maybe a
fromAny (Any r v)
  = do
    HRefl <- r `eqTypeRep` q
    pure v
      where q = (typeRep :: TypeRep a)

-- Next to model type consistency
class Consistent a b where
  cast :: a -> b

instance Consistent a a where
  cast = id

instance (Typeable a) => Consistent a Any where
  cast = toAny

instance (Typeable b) => Consistent Any b where
  cast = fromJust . fromAny

instance {-# OVERLAPPING #-} Consistent Any Any where
  cast = id

-- wrap rule
instance {-# OVERLAPPING #-} (Consistent c a, Consistent b d) => Consistent (a->b) (c->d) where
  cast f = g
    where
      g :: c -> d
      g x = cast @(b) @(d) (f (cast @(c) @(a) x))

-- Special behavriour when casting from Any to (Any -> Any)
instance {-# OVERLAPPING #-} Consistent Any (Any -> Any) where
  cast (Any r f)
    = case r `eqTypeRep` TRFun (typeRep :: TypeRep Any) (typeRep :: TypeRep Any) of
        Just HRefl -> f
        Nothing -> error "not a function"

-- Special behaviour when casting from (Any -> Any) to Any
-- WARNING!: Necessary to prevent circular aplications of (a->b) to Any
instance {-# OVERLAPPING #-} Consistent (Any -> Any) Any where
  cast = toAny

-- Decomposing a cast from Any to (a->b)
instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => Consistent Any (a->b) where
  cast f = cast @(Any -> Any) @(a -> b) (cast @(Any) @(Any -> Any) f)

-- Decomposig a cast from (a->b) to Any
instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => Consistent (a->b) Any where
  cast f = cast @(Any -> Any) @(Any) (cast @(a -> b) @(Any -> Any) f)

-- Static Test Suite
should_compile1 = cast @(Any) @(Any) undefined
should_compile2 = cast @(Any) @(Any->Any) undefined
should_compile3 = cast @(Any->Any) @(Any) undefined


-- Dynamic Test Suite
-- Casting a number
testAnyAndBack = TestCase $ assertEqual
  "Cast to Any should be reversible"
  (3 :: Integer)
  (cast @(Any) @(Integer) (cast @(Integer) @(Any) 3))

testAnyAndBack' = TestCase $ assertEqual
  "Cast a function to any should be reversible"
  (f 5)
  (f' 5)
    where f = (+5)
          f' = cast @(Any) @(Integer -> Integer) (cast @(Integer -> Integer) @(Any) f)

testApplyAnyToAny = TestCase $ assertEqual
  "Applying Any -> Any should work"
  a
  b
    where a = f 5
          f = (+5)
          b = cast @(Any) @(Integer) (g c)
          c = cast @(Integer) @(Any) 5
          g = cast @(Integer -> Integer) @(Any -> Any) f

testApplyAnyToAny' = TestCase $ assertEqual
  "I should be able to cast a fun to Any then Any->Any and apply it"
  a
  b
    where a = f 5
          f = (+5)
          b = cast @(Any) @(Integer) (h c)
          c = cast @(Integer) @(Any) 5
          g = cast @(Integer -> Integer) @(Any) f
          h = cast @(Any) @(Any -> Any) g

testApplyAnyToAny'' = TestCase $ assertEqual
  "I should be able to cast a fun to Any -> Any then to Any and apply it"
  a
  b
    where a = f 5
          b = i 5
          f = (+5)
          g = cast @(Integer -> Integer) @(Any -> Any) f
          h = cast @(Any -> Any) @(Any) g
          i = cast @(Any) @(Integer -> Integer) h

main :: IO Counts
main = runTestTT $ TestList
  [ testAnyAndBack
  , testAnyAndBack'
  , testApplyAnyToAny
  , testApplyAnyToAny'
  , testApplyAnyToAny''
  ]
