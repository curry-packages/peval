module litfree ( litfree.const0, litfree.goal, litfree.main ) where

import Prelude

litfree.const0 :: a -> Prelude.Int
litfree.const0 v1 = 0

litfree.goal :: Prelude.Int -> a
litfree.goal v1 = litfree._pe0 v1

litfree.main :: a
litfree.main = litfree.goal 0

litfree._pe0 :: Prelude.Int -> a
litfree._pe0 v1 = fcase v1 of
    0 -> let v2 free in v2
