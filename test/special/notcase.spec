module notcase ( notcase.not, notcase.main ) where

import Prelude

notcase.not :: Prelude.Bool -> Prelude.Bool
notcase.not v1 = fcase v1 of
    Prelude.False -> Prelude.True
    Prelude.True -> Prelude.False

notcase.main :: Prelude.Bool -> Prelude.Bool
notcase.main v1 = notcase._pe0 v1

notcase._pe0 :: Prelude.Bool -> Prelude.Bool
notcase._pe0 v1 = fcase v1 of
    Prelude.True -> Prelude.True
    Prelude.False -> Prelude.False
