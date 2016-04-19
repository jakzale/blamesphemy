{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Safe where

import Type.Reflection

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

class (Typeable a, Typeable b) => Consistent a b where
  cast :: a -> Maybe b

instance (Typeable a) => Consistent a a where
  cast = pure

instance (Typeable a) => Consistent a Any where
  cast = pure . toAny

instance (Typeable b) => Consistent Any b where
  cast = fromAny

instance (Consistent c a, Consistent b d) => Consistent (a -> Maybe b) (c -> Maybe d) where
  cast f = pure g
    where g :: c -> Maybe d
          g x = do
            v <- cast @(c) @(a) x
            y <- f v
            cast @(b) @(d) y

instance (Consistent a Any, Consistent b Any) => Consistent (a -> Maybe b) Any where
  cast f = do
    f' <- cast @(a -> Maybe b) @(Any -> Maybe Any) f
    cast @(Any -> Maybe Any) @(Any) f'


instance {-# OVERLAPPING #-} Consistent (Any -> Maybe Any) Any where
  cast = pure . toAny

instance {-# OVERLAPPING #-} Consistent Any (Any -> Maybe Any) where
  cast = fromAny

instance {-# OVERLAPPING #-} Consistent Any Any where
  cast = pure

-- Static Test Suite

should_compile1 :: Any -> Maybe Any
should_compile1 = cast @(Any) @(Any)

should_compile2 :: Any -> Maybe (Any -> Maybe Any)
should_compile2 = cast @(Any) @(Any -> Maybe Any)

should_compile3 :: (Any -> Maybe Any) -> Maybe Any
should_compile3 = cast @(Any -> Maybe Any) @(Any)

should_compile4 :: forall a b c d. (Consistent c a, Consistent b d) => (a -> Maybe b) -> Maybe (c -> Maybe d)
should_compile4 = cast @(a -> Maybe b) @(c -> Maybe d)
