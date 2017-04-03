module amp07 ( amp07.PEVAL, amp07.goal, amp07.main ) where

import Prelude

amp07.PEVAL :: a -> a
amp07.PEVAL v1 = v1

amp07.goal :: Prelude.Bool -> Prelude.Bool
amp07.goal v1 = amp07._pe0 v1

amp07.main :: Prelude.Bool
amp07.main = amp07.goal (Prelude.success ? Prelude.failed)

amp07._pe0 :: a -> a
amp07._pe0 v1 = v1
