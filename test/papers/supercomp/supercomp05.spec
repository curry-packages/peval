module supercomp05 ( supercomp05.f, supercomp05.main ) where

import Prelude

supercomp05.f :: a -> Prelude.Int
supercomp05.f v1 = 1 + (supercomp05.f v1)

supercomp05.main :: Prelude.Int
supercomp05.main = supercomp05._pe0

supercomp05._pe0 :: Prelude.Int
supercomp05._pe0 = 1 + supercomp05._pe0
