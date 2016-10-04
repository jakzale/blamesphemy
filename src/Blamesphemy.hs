{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Blamesphemy where

import GHC.TypeLits
import Type.Reflection 
import Data.Proxy (Proxy(Proxy))

-- Simple implementation of boxed type Any
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

-- Computation to figureout how to perform gradual typing
data Cast = FromAny
          | ToAny
          | Same

type family How a b :: Cast where
  How Any Any = Same
  How a Any   = ToAny
  How Any b   = fromAny
  How a   a   = Same
  How a   b   =
    TypeError (Text "Casting between inconsistent types:" :$$:
                (Text "cast from " :<>: ShowType a :<>: Text " to "
                  :<>: ShowType b :<>: Text " is not consistent!"))

Castable 
