{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Checker
  ( Cast(..)
  , How
  , Duh
  ) where

import Any
import GHC.TypeLits

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


type family Duh a b :: Cast where
  Duh Any          (Any -> Any) = FromAny
  Duh Any          (a   -> b)   = Grow
  Duh Any          a            = FromAny
  Duh (Any -> Any) Any          = ToAny
  Duh (a -> b)     Any          = Squish
  Duh (a -> b)     (c -> d)     = Fun (Duh c a) (Duh b d)
  Duh a            Any          = ToAny
  Duh a            a            = Same
  Duh a            b            = 
    TypeError (Text "Casting between inconsistent types:" :$$:
                (Text "cast from " :<>: ShowType a :<>: Text " to "
                  :<>: ShowType b :<>: Text " is not consistent!"))
