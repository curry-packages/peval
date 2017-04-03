module cons ( cons.PEVAL, cons.main ) where

import Prelude

cons.PEVAL :: a -> a
cons.PEVAL v1 = v1

cons.main :: Prelude.Maybe Prelude.Bool
cons.main = cons._pe0

cons._pe0 :: Prelude.Maybe Prelude.Bool
cons._pe0 = Prelude.Just (Prelude.True ? Prelude.False)
