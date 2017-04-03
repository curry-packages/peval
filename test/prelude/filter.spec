module filter ( filter.PEVAL, filter.id, filter.filter, filter.main ) where

import Prelude

filter.PEVAL :: a -> a
filter.PEVAL v1 = v1

filter.id :: a -> a
filter.id v1 = v1

filter.filter :: (a -> Prelude.Bool) -> [a] -> [a]
filter.filter v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> case (Prelude.apply v1 v3) of
        Prelude.True -> v3 : (filter.filter v1 v4)
        Prelude.False -> filter.filter v1 v4

filter.main :: [Prelude.Bool]
filter.main = filter._pe0

filter._pe0 :: [Prelude.Bool]
filter._pe0 = Prelude.True : (Prelude.True : [])
