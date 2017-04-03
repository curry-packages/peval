module base08 ( base08.id, base08.main ) where

import Prelude

base08.id :: a -> a
base08.id v1 = v1

base08.main :: a -> a -> Prelude.Int
base08.main v1 v2 = base08._pe0 v1 v2

base08._pe0 :: a -> a -> Prelude.Int
base08._pe0 v1 v2 = case (v1 Prelude.=:= v2) of
    Prelude.True -> 42
