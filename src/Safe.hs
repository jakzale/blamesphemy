{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Safe where

import Type.Reflection
import Data.Proxy
import GHC.TypeLits


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

data Cast = Same
          | ToAny
          | FromAny
          | Fun Cast Cast
          | Squish
          | Grow

type family How a b :: Cast where
  How Any                (Any -> Maybe Any) = FromAny
  How Any                (a   -> Maybe b)   = Grow
  How Any                a                  = FromAny
  How (Any -> Maybe Any) Any                = ToAny
  How (a -> Maybe b)     Any                = Squish
  How (a -> Maybe b)     (c -> Maybe d)     = Fun (How c a) (How b d)
  How a                  Any                = ToAny
  How a                  a                  = Same
  How a                  b                  =
    TypeError (Text "Casting between inconsistent types:" :$$:
                (Text "cast from " :<>: ShowType a :<>: Text " to "
                  :<>: ShowType b :<>: Text " is not consistent!"))


class (Typeable a, Typeable b) => Safer a b (p :: Cast) where
  safer :: Proxy p -> a -> Maybe b

instance (Typeable a) => Safer a a Same where
  safer _ = pure

instance (Typeable a) => Safer a Any ToAny where
  safer _ = pure . toAny

instance (Typeable b) => Safer Any b FromAny where
  safer _ = fromAny

instance (Safer c a p, Safer b d q) => Safer (a -> Maybe b) (c -> Maybe d) (Fun p q) where
  safer _ f = pure g
    where g :: c -> Maybe d
          g x = do
            x' <- safer @(c) @(a) @(p) undefined x
            y  <- f x'
            safer @(b) @(d) @(q) undefined y

 -- This one is slighlty troublesome
instance (Safer Any a FromAny, Safer b Any ToAny) => Safer (a -> Maybe b) Any Squish where
  safer _ f = do
    f' <- safer @(a -> Maybe b) @(Any -> Maybe Any) @(Fun FromAny ToAny) undefined f
    safer @(Any -> Maybe Any) @(Any) @(ToAny) undefined f'

instance (Safer a Any ToAny, Safer Any b FromAny) => Safer Any (a -> Maybe b) Grow where
  safer _ f = do
    f' <- safer @(Any) @(Any -> Maybe Any) @(FromAny) undefined f
    safer @(Any -> Maybe Any) @(a -> Maybe b) @(Fun ToAny FromAny) undefined f'

cast :: forall a b. (Safer a b (How a b)) => a -> Maybe b
cast = safer (undefined :: Proxy (How a b))

-- This bit seems to work
example1 = safer @(Integer) @(Any) @(ToAny) undefined 2
example1' = cast @(Integer) @(Any) 2

example2 = do
  x <- example1
  safer @(Any) @(Integer) @(FromAny) undefined x
example2' = do
  x <- example1'
  cast @(Any) @(Integer) x

example3 = do
  a <- safer @(Integer -> Maybe Integer) @(Any) @(Squish) undefined (pure . (+5))
  b <- safer @(Any) @(Any -> Maybe Any) @(FromAny) undefined a
  c <- safer @(Any -> Maybe Any) @(Integer -> Maybe Integer) @(Fun ToAny FromAny) undefined b
  c 3
example3' = do
  a <- cast @(Integer -> Maybe Integer) @(Any) (pure . (+5))
  b <- cast @(Any) @(Any -> Maybe Any) a
  c <- cast @(Any -> Maybe Any) @(Integer -> Maybe Integer) b
  c 3

example4 = cast @(Integer) @(Bool) 1
