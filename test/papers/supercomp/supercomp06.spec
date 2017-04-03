module supercomp06
  ( supercomp06.count, supercomp06.goal, supercomp06.main )
  where

import Prelude

supercomp06.count :: Prelude.Int -> [Prelude.Int]
supercomp06.count v1 = v1 : (supercomp06.count (v1 + 1))

supercomp06.goal :: [Prelude.Int]
supercomp06.goal = supercomp06._pe0

supercomp06.main :: [Prelude.Int]
supercomp06.main = Prelude.take 10 supercomp06.goal

supercomp06._pe0 :: [Prelude.Int]
supercomp06._pe0 = 0 : (1 : (supercomp06._pe1 1))

supercomp06._pe1 :: Prelude.Int -> [Prelude.Int]
supercomp06._pe1 v1 = let { v2 = v1 + 1 } in v2 : (supercomp06._pe1 v2)
