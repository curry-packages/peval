module abstraction ( abstraction.f, abstraction.goal1, abstraction.goal2 ) where

import Prelude

abstraction.f :: Prelude.Bool -> Prelude.Int -> a
abstraction.f v1 v2 = fcase v1 of
    Prelude.True -> abstraction.f Prelude.True (v2 + 1)
    Prelude.False -> abstraction.f Prelude.False (v2 + 1)

abstraction.goal1 :: a
abstraction.goal1 = abstraction._pe0

abstraction.goal2 :: a
abstraction.goal2 = abstraction._pe0

abstraction._pe0 :: a
abstraction._pe0 = abstraction._pe1 2

abstraction._pe1 :: Prelude.Int -> a
abstraction._pe1 v1 = abstraction._pe1 (v1 + 1)
