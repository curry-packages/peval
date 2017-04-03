module amp08 ( amp08.PEVAL, amp08.goal, amp08.main ) where

import Prelude

amp08.PEVAL :: a -> a
amp08.PEVAL v1 = v1

amp08.goal :: Prelude.Bool -> Prelude.Bool
amp08.goal v1 = amp08._pe0 v1

amp08.main :: Prelude.Bool
amp08.main = amp08.goal (Prelude.success ? Prelude.failed)

amp08._pe0 :: a -> b
amp08._pe0 v1 = Prelude.failed
