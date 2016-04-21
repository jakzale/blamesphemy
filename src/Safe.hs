{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Safe where

import Type.Reflection

data Any where
  Any :: forall a. TypeRep a -> a -> Any

toAny :: forall a. (Typeable a) => a -> Any
toAny v = Any typeRep v

fromAny :: forall a. (Typeable a) => Any -> Maybe a
fromAny (Any r v)
  = do
    HRefl <- r `eqTypeRep` q
    pure v
      where q = (typeRep :: TypeRep a)

data Same
data ToAny
data FromAny
data Fun
data Squish
data Grow

class (Typeable a, Typeable b) => Safer a b p where
  safer :: p -> a -> Maybe b

instance (Typeable a) => Safer a a Same where
  safer _ = pure

instance (Typeable a) => Safer a Any ToAny where
  safer _ = pure . toAny

instance (Typeable b) => Safer Any b FromAny where
  safer _ = fromAny

instance (Safer c a p, Safer b d q) => Safer (a -> Maybe b) (c -> Maybe d) Fun where
  safer _ f = pure g
    where g :: c -> Maybe d
          g x = do
            x' <- safer @(c) @(a) @(p) undefined x
            y  <- f x'
            safer @(b) @(d) @(q) undefined y

 {- Not sure if this bit is necessary 
instance Safer (Any -> Maybe Any) Any Squish where
  safer _ f = 
  -}

-- This bit seems to work
example1 = safer @(Integer) @(Any) @(ToAny) undefined 2
example2 = do
  x <- example1
  safer @(Any) @(Integer) @(FromAny) undefined x

