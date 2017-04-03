module amp10 ( amp10.PEVAL, amp10.goal, amp10.main ) where

import Prelude

amp10.PEVAL :: a -> a
amp10.PEVAL v1 = v1

amp10.goal :: Prelude.Bool -> Prelude.Bool
amp10.goal v1 = amp10._pe0 v1

amp10.main :: Prelude.Bool
amp10.main = (amp10.goal Prelude.True) ? (amp10.goal Prelude.False)

amp10._pe0 :: Prelude.Bool -> Prelude.Bool
amp10._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
