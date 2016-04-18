{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Safe where

 {-
  Looks nice, but unfortunately bog useless :P.

  Still working on the implementation of safe casts...

  I thought that it could be a simple monad, but maybe I need to use a GADT to
  handle that.  For instance, I need a way to cast from a -> b to Any -> Any
  and still manage it somehow.

  I guess I will read about type and effect systems for now...

  -}

import Type.Reflection
import Data.Maybe

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

instance Functor Blame where
  fmap f a = MkBlame $ do
    b <- unBlame a
    pure $ f b

instance Applicative Blame where
  pure    = MkBlame . pure
  f <*> a = MkBlame $ do
    g <- unBlame f
    b <- unBlame a
    pure $ g b

instance Monad Blame where
  a >>= f = MkBlame $ do
    b <- unBlame a
    unBlame $ f b

class (Typeable a, Typeable b) => Consistent a b where
  cast :: a -> b

instance (Typeable a) => Consistent (Blame a) (Blame Any) where
  cast x = do
    v <- x
    MkBlame . pure $ toAny v

instance (Typeable b) => Consistent (Blame Any) (Blame b) where
  cast x = do
    v <- x
    MkBlame $ fromAny v

instance (Typeable a, Typeable b, Typeable c, Typeable d, Consistent (Blame c) (Blame a), Consistent (Blame b) (Blame d)) => Consistent (Blame a -> Blame b) (Blame c -> Blame d) where
  cast f = cast @(Blame b) @(Blame d) . f . cast @(Blame c) @(Blame a)

instance Consistent (Blame (Any -> Any)) (Blame Any) where
  cast x = do
    f <- x
    MkBlame . pure . toAny $ f

-- This cast is consistent
instance Consistent (Blame Any) (Blame (Any -> Any)) where
  cast x = do
    v <- x
    MkBlame . fromAny $ v

foo :: Integer -> Integer
foo = (+5)

fooB :: Blame Integer -> Blame Integer
fooB = (<$>) foo

fooA :: Blame Any -> Blame Any
fooA = cast fooB

iB :: Blame Integer
iB = pure 3

iA :: Blame Any
iA = cast iB

test :: Maybe Integer
test = unBlame $ cast $ fooA iA

