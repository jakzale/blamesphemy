{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Unsafe version of the gradually typed calculus
module Unsafe where

import Type.Reflection
import Data.Maybe
import Data.Proxy
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

data Constraint = Same
                | ToAny
                | FromAny
                | Fun Constraint Constraint
                | Squish
                | Grow


type family How a b :: Constraint where
  How Any          (Any -> Any) = FromAny
  How Any          (a   -> b)   = Grow
  How Any          a            = FromAny
  How (Any -> Any) Any          = ToAny
  How (a -> b)     Any          = Squish
  How (a -> b)     (c -> d)     = Fun (How c a) (How b d)
  How a            Any          = ToAny
  How a            a            = Same

class (Typeable a, Typeable b) => Unsafer a b (p :: Constraint) where
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
          g x = unsafer @(b) @(d) @(q) undefined y
            where
              x'  = unsafer @(c) @(a) @(p) undefined x
              y   = f x'

instance (Unsafer Any a FromAny, Unsafer b Any ToAny) => Unsafer (a -> b) Any Squish where
  unsafer _ f = g
    where
      f' = unsafer @(a -> b) @(Any -> Any) @(Fun FromAny ToAny) undefined f 
      g  = unsafer @(Any -> Any) @(Any) @(ToAny) undefined f'
    
instance (Unsafer a Any ToAny, Unsafer Any b FromAny) => Unsafer Any (a -> b) Grow where
  unsafer _ f = g
    where
      f' = unsafer @(Any) @(Any -> Any) @(FromAny) undefined f
      g  = unsafer @(Any -> Any) @(a -> b) @(Fun ToAny FromAny) undefined f'

cast :: forall a b. (Unsafer a b (How a b)) => a -> b
cast = unsafer (undefined :: Proxy (How a b))

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
