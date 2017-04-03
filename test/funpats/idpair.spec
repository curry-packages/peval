module idpair ( idpair.idpair, idpair.f, idpair.main ) where

import Prelude

idpair.idpair :: a -> (a,a)
idpair.idpair v1 = (v1,v1)

idpair.f :: (a,a) -> Prelude.Int
idpair.f v1 = idpair._pe0 v1

idpair.main :: Prelude.Int
idpair.main = (idpair.f (idpair.idpair Prelude.True))
  ?
  (idpair.f (Prelude.True,Prelude.False))

idpair._pe0 :: (a,a) -> Prelude.Int
idpair._pe0 v1 = fcase v1 of
    (v2,v3) -> (v2 Prelude.=:= v3) &> 0
