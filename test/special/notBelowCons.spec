module notBelowCons ( notBelowCons.main ) where

import Prelude

notBelowCons.main :: (Prelude.Bool,Prelude.Bool)
notBelowCons.main = let v1 free in (notBelowCons._pe1 v1,notBelowCons._pe0 v1)

notBelowCons._pe0 :: Prelude.Bool -> Prelude.Bool
notBelowCons._pe0 v1 = fcase v1 of
    Prelude.False -> Prelude.True

notBelowCons._pe1 :: Prelude.Bool -> Prelude.Bool
notBelowCons._pe1 v1 = fcase v1 of
    Prelude.True -> Prelude.True
