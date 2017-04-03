module sharefail ( sharefail.f, sharefail.g, sharefail.main ) where

import Prelude

sharefail.f :: Prelude.Bool -> Prelude.Bool
sharefail.f v1 = fcase v1 of
    Prelude.True -> Prelude.True

sharefail.g :: Prelude.Bool -> (Prelude.Bool,Prelude.Bool)
sharefail.g v1 = (sharefail.f v1,sharefail.f v1)

sharefail.main :: (Prelude.Bool,Prelude.Bool)
sharefail.main = sharefail._pe0

sharefail._pe0 :: (Prelude.Bool,Prelude.Bool)
sharefail._pe0 = let { v1 = Prelude.False ? Prelude.True }
  in (fcase v1 of
       Prelude.True -> Prelude.True
     ,fcase v1 of
       Prelude.True -> Prelude.True)
