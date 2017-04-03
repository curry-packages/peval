module not ( not.PEVAL, not.not, not.main ) where

import Prelude

not.PEVAL :: a -> a
not.PEVAL v1 = v1

not.not :: Prelude.Bool -> Prelude.Bool
not.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

not.main :: [Prelude.Bool]
not.main = not._pe0

not._pe0 :: [Prelude.Bool]
not._pe0 = Prelude.False : (Prelude.True : [])
