module matchCons
  ( matchCons.PEVAL, matchCons.f, matchCons.goal, matchCons.main )
  where

import Prelude

matchCons.PEVAL :: a -> a
matchCons.PEVAL v1 = v1

matchCons.f :: Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int
matchCons.f v1 = fcase v1 of
    Prelude.Nothing -> Prelude.Nothing
    Prelude.Just v2 -> Prelude.Just (v2 + v2)

matchCons.goal :: Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int
matchCons.goal v1 = matchCons.f v1

matchCons.main :: (Prelude.Maybe Prelude.Int,Prelude.Maybe Prelude.Int)
matchCons.main = (matchCons.goal Prelude.Nothing
                 ,matchCons.goal (Prelude.Just 1))
