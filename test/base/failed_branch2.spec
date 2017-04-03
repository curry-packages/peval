module failed_branch2
  ( failed_branch2.PEVAL, (failed_branch2.&&), failed_branch2.goal
  , failed_branch2.main )
  where

import Prelude

failed_branch2.PEVAL :: a -> a
failed_branch2.PEVAL v1 = v1

(failed_branch2.&&) :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
(failed_branch2.&&) v1 v2 = fcase v1 of
    Prelude.True -> v2
    Prelude.False -> Prelude.False

failed_branch2.goal :: Prelude.Bool -> a
failed_branch2.goal v1 = failed_branch2._pe0 v1

failed_branch2.main :: a
failed_branch2.main = (failed_branch2.goal Prelude.True)
  ?
  (failed_branch2.goal Prelude.False)

failed_branch2._pe0 :: a -> b
failed_branch2._pe0 v1 = Prelude.failed
