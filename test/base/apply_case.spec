module apply_case
  ( apply_case.PEVAL, apply_case.id, apply_case.not, apply_case.app
  , apply_case.goal, apply_case.main )
  where

import Prelude

apply_case.PEVAL :: a -> a
apply_case.PEVAL v1 = v1

apply_case.id :: a -> a
apply_case.id v1 = v1

apply_case.not :: Prelude.Bool -> Prelude.Bool
apply_case.not v1 = fcase v1 of
    Prelude.True -> Prelude.False
    Prelude.False -> Prelude.True

apply_case.app :: (a -> b) -> a -> b
apply_case.app v1 v2 = Prelude.apply v1 v2

apply_case.goal :: Prelude.Bool -> Prelude.Bool
apply_case.goal v1 = apply_case._pe0 v1

apply_case.main :: (Prelude.Bool,Prelude.Bool)
apply_case.main = (apply_case.goal Prelude.True,apply_case.goal Prelude.False)

apply_case._pe0 :: Prelude.Bool -> Prelude.Bool
apply_case._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
    Prelude.False -> Prelude.False
