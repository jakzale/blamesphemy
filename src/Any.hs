{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Any
  ( Any()
  , toAny
  , fromAny
  ) where

-- Technically speaking, this bit is redundant, as it could be replaced by
-- Data.Dynamic.

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
