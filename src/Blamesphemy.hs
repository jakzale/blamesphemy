{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blamesphemy where

import Type.Reflection
-- import Data.Proxy

aList = typeRep @[Int]
aFunction = typeRep @(Int -> Char)

-- My own implementation of Dynamic

data Any where
  MkAny :: TypeRep a -> a -> Any

toAny :: Typeable a => a -> Any
toAny v = MkAny typeRep v

fromAny :: forall d. Typeable d => Any -> Maybe d
fromAny (MkAny (ra :: TypeRep a) (x :: a))
  = do
    HRefl <- ra `eqTypeRep` (typeRep :: TypeRep d)
    return x

-- Implementation of cast from Data.Typeable
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x
  = do
    HRefl <- (typeRep :: TypeRep a) `eqTypeRep` (typeRep :: TypeRep b)
    return x

foo :: Integer
foo = 42

foo' :: Maybe Any
foo' = cast @(Integer) @(Any) foo

foo'' :: Maybe Integer
foo'' = foo' >>= cast @(Any) @(Integer)

bcast :: forall a b c d. (Typeable a, Typeable b) => a -> (c -> b) -> (d -> b) -> b
bcast v p q
  = case (typeRep :: TypeRep a) `eqTypeRep` (typeRep :: TypeRep b) of
      Nothing    -> (p undefined)
      Just HRefl -> v

example1 :: Integer
example1 = bcast (42 :: Integer) (error "positive blame") (error "negative blame")
-- 42

example2 :: Bool
example2 = bcast (42 :: Integer) (error "positive blame") (error "negative blame")
-- "positive blame"

-- "Here comes the blasphemy!"
isFun :: forall a. (Typeable a) => a -> Bool
isFun v = case (typeRep :: TypeRep a) of
            TRFun _ _ -> True
            otherwise -> False

