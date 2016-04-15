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

{-
-- Ensure that whatever is inside will be first cast to * -> *
instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => Consistent (a -> b) Any where
  cast f = cast @(Any -> Any) @(Any) g
    where
      g :: Any -> Any
      g = cast @(a -> b) @(Any -> Any) f
-}

-- These two are necessary
instance {-# OVERLAPPING #-} Consistent Any (Any -> Any) where
  cast (Any r f)
    = case r `eqTypeRep` TRFun (typeRep :: TypeRep Any) (typeRep :: TypeRep Any) of
        Just HRefl -> f
        Nothing -> error "not a function"

instance {-# OVERLAPPING #-} Consistent (Any -> Any) Any where
  cast = toAny

{-
 -- This bit type checks.  But it is not really useful...
instance (Typeable a, Typeable b) => Consistent Any (a -> b) where
  cast (Any r f)
    = case r `eqTypeRep` TRFun ra rb of
        Just HRefl -> f
        Nothing -> error "not a function"
      where
        ra :: TypeRep a
        ra = typeRep
        rb :: TypeRep b
        rb = typeRep
-}

{-
-- This one is a bit frustrating...
instance (Typeable a, Typeable b) => Consistent Any (a -> b) where
  cast (Any r f)
    = case r of
        TRFun (ra :: TypeRep arg) (rb :: TypeRep res) ->
          -- Unifying types for r and (arg -> res)
          -- This bit will not work, because it does not know that both of them are typeable
          cast @(arg -> res) @(a -> b) f
        otherwise -> error "not a function"
-}

-- Find a way to prevent this from overlapping


-- Static Test Suite
should_compile1 = cast @(Any) @(Any) undefined
should_compile2 = cast @(Any) @(Any->Any) undefined
should_compile3 = cast @(Any->Any) @(Any) undefined


-- Dynamic Test Suite
-- Casting a number
aAny :: Any
aAny = cast @(Integer) @(Any) 5

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

foo :: Integer -> Integer
foo = (+3)

fooAnyToAny :: Any -> Any
fooAnyToAny = cast foo

fooAny :: Any
fooAny = cast fooAnyToAny

-- When Applying Any
bAny :: Any
bAny = fooAnyToAny aAny

b :: Integer
b = cast bAny

-- When Applying Any'
fooAny' :: Any
fooAny' = cast @(Integer->Integer) @(Any) foo

foo' :: Integer -> Integer
foo' = cast fooAny'

bAny' :: Any
bAny' = (cast @(Any) @(Any -> Any) fooAny') aAny

b' :: Integer
b' = cast bAny'

main :: IO Counts
main = runTestTT $ TestList
  [ testAnyAndBack
  , testAnyAndBack'
  , testApplyAnyToAny
  ]
