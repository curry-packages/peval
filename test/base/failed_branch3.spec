module failed_branch3
  ( failed_branch3.PEVAL, (failed_branch3.&&), failed_branch3.goal
  , failed_branch3.main )
  where

import Prelude

failed_branch3.PEVAL :: a -> a
failed_branch3.PEVAL v1 = v1

(failed_branch3.&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(failed_branch3.&&) v1 v2 = fcase v1 of
    Prelude.True -> v2
    Prelude.False -> Prelude.False

failed_branch3.goal :: Prelude.Bool -> Prelude.Bool
failed_branch3.goal v1 = failed_branch3._pe0 v1

failed_branch3.main :: Prelude.Bool
failed_branch3.main = (failed_branch3.goal Prelude.True)
  ?
  (failed_branch3.goal Prelude.False)

failed_branch3._pe0 :: a -> b
failed_branch3._pe0 v1 = Prelude.failed
