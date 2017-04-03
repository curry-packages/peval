module amp11 ( amp11.PEVAL, amp11.goal, amp11.main ) where

import Prelude

amp11.PEVAL :: a -> a
amp11.PEVAL v1 = v1

amp11.goal :: Prelude.Bool -> Prelude.Bool
amp11.goal v1 = amp11._pe0 v1

amp11.main :: Prelude.Bool
amp11.main = (amp11.goal Prelude.True) ? (amp11.goal Prelude.False)

amp11._pe0 :: a -> b
amp11._pe0 v1 = Prelude.failed
