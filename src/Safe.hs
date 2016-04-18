{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Safe where

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

newtype Blame a = MkBlame { unBlame :: Maybe a }

-- My conjecture is that this is similar to cast on base types...
instance Functor Blame where
  fmap f a = MkBlame $ do
    b <- unBlame a
    pure $ f b

-- My conjecture is that this is similar to a cast between function types...
instance Applicative Blame where
  pure    = MkBlame . pure
  f <*> a = MkBlame $ do
    g <- unBlame f
    b <- unBlame a
    pure $ g b

-- My conjecture is that is similar to a cast between Any => (Any -> Any)...
instance Monad Blame where
  a >>= f = MkBlame $ do
    b <- unBlame a
    unBlame $ f b

-- Simple implementation of cast
cast :: (Typeable a, Typeable b) => a -> Blame b
cast = MkBlame . fromAny . toAny

