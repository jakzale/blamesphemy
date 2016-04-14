{-#  LANGUAGE TypeApplications, GADTs #-}

module Blamesphemy where

import Type.Reflection
-- import Data.Proxy

aList = typeRep @[Int]
aFunction = typeRep @(Int -> Char)

-- My own implementation of Dynamic

data Any where
  MkAny :: TypeRep a -> a -> Any

toAny :: Typeable a => a -> Any
toAny a = MkAny typeRep a

{-
fromAny :: Typeable a => Any -> Maybe a
fromAny (MkAny (ra :: TypeRep a) (x :: a))
  = Just Refl <- ra `eqTypeRep` rep
-}
