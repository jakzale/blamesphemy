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

-- Looks awfully similar to the cast, especially cast on base types
instance Functor Blame where
  fmap f a = MkBlame $ do
    b <- unBlame a
    pure $ f b

-- Looks awfully similar to the cast, especially cast on functions
instance Applicative Blame where
  pure    = MkBlame . pure
  f <*> a = MkBlame $ do
    g <- unBlame f
    b <- unBlame a
    pure $ g b

-- This one also looks awfully similar to cast
instance Monad Blame where
  a >>= f = MkBlame $ do
    b <- unBlame a
    unBlame $ f b
