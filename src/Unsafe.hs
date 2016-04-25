{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- Unsafe version of the gradually typed calculus
module Unsafe
  ( cast
  ) where

import Type.Reflection (Typeable, TypeRep, typeRep)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Checker
import Any
import Test.HUnit


class (Typeable a, Typeable b) => Unsafer a b (p :: Cast) where
  unsafer :: Proxy p -> a -> b

instance (Typeable a) => Unsafer a a Same where
  unsafer _ = id

instance (Typeable a) => Unsafer a Any ToAny where
  unsafer _ = toAny

instance (Typeable b) => Unsafer Any b FromAny where
  unsafer _ = fromJust . fromAny

instance (Unsafer c a p, Unsafer b d q) => Unsafer (a -> b) (c -> d) (Fun p q) where
  unsafer _ f = g
    where g :: c -> d
          g x = unsafer @(b) @(d) @(q) Proxy y
            where
              x'  = unsafer @(c) @(a) @(p) Proxy x
              y   = f x'

instance (Unsafer Any a FromAny, Unsafer b Any ToAny) => Unsafer (a -> b) Any Squish where
  unsafer _ f = g
    where
      f' = unsafer @(a -> b) @(Any -> Any) @(Fun FromAny ToAny) Proxy f
      g  = unsafer @(Any -> Any) @(Any) @(ToAny) Proxy f'

instance (Unsafer a Any ToAny, Unsafer Any b FromAny) => Unsafer Any (a -> b) Grow where
  unsafer _ f = g
    where
      f' = unsafer @(Any) @(Any -> Any) @(FromAny) Proxy f
      g  = unsafer @(Any -> Any) @(a -> b) @(Fun ToAny FromAny) Proxy f'

cast :: forall a b. (Unsafer a b (Duh a b)) => a -> b
cast = unsafer (Proxy :: Proxy (Duh a b))

-- Static Test Suite
should_compile1 :: Any -> Any
should_compile1 = cast @(Any) @(Any)

should_compile2 :: Any -> (Any -> Any)
should_compile2 = cast @(Any) @(Any -> Any)

should_compile3 :: (Any -> Any) -> Any
should_compile3 = cast @(Any->Any) @(Any)


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

testIdemCast = TestCase $ assertEqual
  "I should be able to cast Integer to an Integer"
  a
  b
    where
      a = 5
      b = cast @(Integer) @(Integer) 5

testIdemCast' = TestCase $ assertEqual
  "I should be able to cast (a->b) to (a->b)"
  a
  b
    where
      a = f 5
      b = g 5
      f = (+ 5)
      g = cast @(Integer -> Integer) @(Integer -> Integer) f

main :: IO Counts
main = runTestTT $ TestList
  [ testAnyAndBack
  , testAnyAndBack'
  , testApplyAnyToAny
  , testApplyAnyToAny'
  , testApplyAnyToAny''
  , testIdemCast
  , testIdemCast'
  ]
