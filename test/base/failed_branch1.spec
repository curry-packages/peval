module failed_branch1
  ( failed_branch1.PEVAL, (failed_branch1.&&), failed_branch1.goal
  , failed_branch1.main )
  where

import Prelude

failed_branch1.PEVAL :: a -> a
failed_branch1.PEVAL v1 = v1

(failed_branch1.&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(failed_branch1.&&) v1 v2 = fcase v1 of
    Prelude.True -> v2
    Prelude.False -> Prelude.False

failed_branch1.goal :: Prelude.Bool -> Prelude.Bool
failed_branch1.goal v1 = failed_branch1._pe0 v1

failed_branch1.main :: Prelude.Bool
failed_branch1.main = (failed_branch1.goal Prelude.True)
  ?
  (failed_branch1.goal Prelude.False)

failed_branch1._pe0 :: Prelude.Bool -> Prelude.Bool
failed_branch1._pe0 v1 = case v1 of
    Prelude.True -> Prelude.True
