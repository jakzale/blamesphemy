{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

 {-
  Some reading material
  https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29 
  -}

module Blamesphemy where

import Type.Reflection
import Data.Maybe
import GHC.Exts hiding (Any)

data Any :: * where
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

instance Typeable a => Consistent a Any where
  cast = undefined

instance Typeable b => Consistent Any b where
  cast = undefined

-- Find a way to prevent this from overlapping
test = cast @(Any) @(Any) undefined

{-


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
