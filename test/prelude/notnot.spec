module notnot ( notnot.not, notnot.main ) where

import Prelude

notnot.not :: Prelude.Bool -> Prelude.Bool
notnot.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

notnot.main :: Prelude.Bool -> Prelude.Bool
notnot.main v1 = notnot._pe0 v1

notnot._pe0 :: Prelude.Bool -> Prelude.Bool
notnot._pe0 v1 = fcase v1 of
    Prelude.True -> Prelude.True
    Prelude.False -> Prelude.False
