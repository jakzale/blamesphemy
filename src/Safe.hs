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

type family Safer (a :: *) :: * where
  Safer (a -> b) = (Safer a) -> (Safer b)
  Safer a        = Maybe a

foo :: Integer -> Integer
foo = (+5)

class (Typeable a, Typeable b) => Consistent a b where
  cast :: forall c. (c ~ Safer b) => a -> c

instance (Typeable a) => Consistent a a where
  cast = undefined

instance {-# INCOHERENT #-} (Consistent c a, Consistent b d) => Consistent (a -> b) (c -> d) where
  cast = undefined 

-- Test program
test :: Maybe Integer
test = cast @(Integer) @(Integer) undefined

test' :: Maybe Integer -> Maybe Integer
test' = cast @(Integer -> Integer) @(Integer -> Integer) undefined

type family S (a :: *) :: * where
  S (a -> b) = Maybe (S' a -> S b) 
  S a        = Maybe a

type family S' (a :: *) :: * where
  S' (a -> b) = S (a -> b)
  S' a        = a

class (Typeable a, Typeable b) => Cons a b where
  cst :: a -> S b

instance Cons Any Any where
  cst = pure

instance Cons Any (Any -> Any) where
  cst a = (undefined :: Maybe (Any -> Maybe Any))

{-

-- I am not sure if this is decidable...
instance (Cons c a, Cons b d) => Cons (a -> b) (c -> d) where
  cst f = pure g
    where
      g :: S' c -> S d
      g x = do
        y <- cst @(c) @(a) _
        cst @(b) @(d) (f y)
-}

instance (Cons c a, Cons b d) => Cons (a -> b) (c -> d) where
  cst f = undefined

-- instance 
{-
instance Consistent a a where
  -- cast :: S a ~ Maybe a => a -> S a
  cast x = pure _
-}

{-
-- This instance is incoherent, as it is the most general, but it should always hold first!
instance {-# INCOHERENT #-} Consistent a a where
  cast :: a -> Maybe a
  cast = pure
-}

{-
instance (Typeable a) => Consistent a Any where
  cast = pure . toAny

instance (Typeable b) => Consistent Any b where
  cast = fromAny

instance {-# OVERLAPPING #-} Consistent Any Any where
  cast = pure

-- wrap rule
instance (Consistent c a, Consistent b d) => Consistent (a -> b) (c -> d) where
  cast f = pure g
    where
      g :: c -> Maybe d
      g x = do
        y <- cast @(c) @(a) x
        cast @(b) @(d) (f y)

-- Special behavriour when casting from Any to (Any -> Any)
instance {-# OVERLAPPING #-} Consistent Any (Any -> Any) where
  cast (Any r f)
    = do
      HRefl <- r `eqTypeRep` TRFun any many
      pure f
        where any  = typeRep @(Any)
              many = typeRep @(Maybe Any) 

-- Special behaviour when casting from (Any -> Any) to Any
-- WARNING!: Necessary to prevent circular aplications of (a->b) to Any
instance {-# OVERLAPPING #-} Consistent (Any -> Any) Any where
  cast = pure . toAny

-- Decomposing a cast from Any to (a->b)
instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => Consistent Any (a -> b) where
  cast f = do
    g <- cast @(Any) @(Any -> Any) f
    _

  -- cast @(Any -> Any) @(a -> b) (cast @(Any) @(Any -> Any) f)

 {-
-- Decomposig a cast from (a->b) to Any
instance {-# OVERLAPPING #-} (Typeable a, Typeable b) => Consistent (a->b) Any where
  cast f = cast @(Any -> Any) @(Any) (cast @(a -> b) @(Any -> Any) f)

-}

-- Static testing
test1 = cast @(Any) @(Maybe Any) undefined
-}
