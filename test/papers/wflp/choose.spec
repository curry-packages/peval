module choose ( choose.foldr, choose.goal, choose.main, choose.benchmark ) where

import Prelude
import Profile
import SetFunctions
import Unsafe

choose.foldr :: (a -> b -> b) -> b -> [a] -> b
choose.foldr v1 v2 v3 = fcase v3 of
    [] -> v2
    v4 : v5 -> Prelude.apply (Prelude.apply v1 v4) (choose.foldr v1 v2 v5)

choose.goal :: [a] -> a
choose.goal v1 = choose._pe0 v1

choose.main :: Prelude.Int
choose.main = choose.goal (Prelude.enumFromTo 1 10)

choose.benchmark :: Prelude.IO ()
choose.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 10000)))
  Prelude.>>
  (Profile.profileTimeNF
  Prelude.$
  (Unsafe.unsafePerformIO
  Prelude.$
  (SetFunctions.values2list Prelude.$ (SetFunctions.set1 choose.goal v1))))

choose._pe0 :: [a] -> a
choose._pe0 v1 = fcase v1 of
    v2 : v3 -> choose._pe1 v2 v3

choose._pe1 :: a -> [a] -> a
choose._pe1 v1 v2 = v1 ? (fcase v2 of
    v3 : v4 -> choose._pe1 v3 v4)
