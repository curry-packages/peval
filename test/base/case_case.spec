module case_case
  ( case_case.ID (..), case_case.PEVAL, case_case.id, case_case.goal
  , case_case.main )
  where

import Prelude

data case_case.ID a
  = case_case.ID a

case_case.PEVAL :: a -> a
case_case.PEVAL v1 = v1

case_case.id :: Prelude.Bool -> case_case.ID Prelude.Bool
case_case.id v1 = fcase v1 of
    Prelude.False -> case_case.ID Prelude.False
    Prelude.True -> case_case.ID Prelude.True

case_case.goal :: Prelude.Bool -> Prelude.Bool
case_case.goal v1 = case_case._pe0 v1

case_case.main :: (Prelude.Bool,Prelude.Bool)
case_case.main = (case_case.goal Prelude.True,case_case.goal Prelude.False)

case_case._pe0 :: Prelude.Bool -> Prelude.Bool
case_case._pe0 v1 = fcase v1 of
    Prelude.False -> Prelude.False
    Prelude.True -> Prelude.True
