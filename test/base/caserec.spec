module caserec ( caserec.f, caserec.goal, caserec.main ) where

import Prelude

caserec.f :: Prelude.Bool -> a -> a
caserec.f v1 v2 = fcase v1 of
    Prelude.True -> v2
    Prelude.False -> v2

caserec.goal :: Prelude.Bool -> Prelude.Int
caserec.goal v1 = caserec._pe0 v1

caserec.main :: (Prelude.Int,Prelude.Int)
caserec.main = (caserec.goal Prelude.True,caserec.goal Prelude.False)

caserec._pe0 :: Prelude.Bool -> Prelude.Int
caserec._pe0 v1 = fcase v1 of
    Prelude.True -> 1
    Prelude.False -> 1
