module notfree ( notfree.main, notfree.not ) where

import Prelude

notfree.main :: Prelude.Bool
notfree.main = notfree._pe0

notfree.not :: Prelude.Bool -> Prelude.Bool
notfree.not v1 = fcase v1 of
    Prelude.False -> Prelude.True
    Prelude.True -> Prelude.False

notfree._pe0 :: Prelude.Bool
notfree._pe0 = Prelude.True ? Prelude.False
