module headPerm
  ( headPerm.perm, headPerm.insert, headPerm.head, headPerm.goal, headPerm.main
  , headPerm.benchmark )
  where

import Prelude
import Profile
import SetFunctions
import Unsafe

headPerm.perm :: [a] -> [a]
headPerm.perm v1 = fcase v1 of
    [] -> []
    v2 : v3 -> headPerm.insert v2 (headPerm.perm v3)

headPerm.insert :: a -> [a] -> [a]
headPerm.insert v1 v2 = (fcase v2 of
    v3 : v4 -> v3 : (headPerm.insert v1 v4)) ? (v1 : v2)

headPerm.head :: [a] -> a
headPerm.head v1 = fcase v1 of
    v2 : v3 -> v2

headPerm.goal :: [a] -> a
headPerm.goal v1 = headPerm._pe0 v1

headPerm.main :: Prelude.Int
headPerm.main = headPerm.goal (Prelude.enumFromTo 1 10)

headPerm.benchmark :: Prelude.IO ()
headPerm.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 10000)))
  Prelude.>>
  (Profile.profileTimeNF
  Prelude.$
  (Unsafe.unsafePerformIO
  Prelude.$
  (SetFunctions.values2list Prelude.$ (SetFunctions.set1 headPerm.goal v1))))

headPerm._pe0 :: [a] -> a
headPerm._pe0 v1 = fcase v1 of
    v2 : v3 -> headPerm._pe1 v3 v2

headPerm._pe1 :: [a] -> a -> a
headPerm._pe1 v1 v2 = (fcase v1 of
    v3 : v4 -> headPerm._pe1 v4 v3) ? v2
