module normalization
  ( normalization.const, normalization.id, normalization.goal1
  , normalization.goal2 )
  where

import Prelude

normalization.const :: a -> b -> a
normalization.const v1 v2 = v1

normalization.id :: a -> a
normalization.id v1 = v1

normalization.goal1 :: a -> (Prelude.Int,Prelude.Int)
normalization.goal1 v1 = normalization._pe0

normalization.goal2 :: a -> (Prelude.Int,Prelude.Int)
normalization.goal2 v1 = normalization._pe0

normalization._pe0 :: (Prelude.Int,Prelude.Int)
normalization._pe0 = (1,0)
