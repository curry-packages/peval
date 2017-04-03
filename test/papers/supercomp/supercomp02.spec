module supercomp02
  ( supercomp02.map, supercomp02.const, supercomp02.goal, supercomp02.main )
  where

import Prelude

supercomp02.map :: (a -> b) -> [a] -> [b]
supercomp02.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (supercomp02.map v1 v4)

supercomp02.const :: a -> b -> a
supercomp02.const v1 v2 = v1

supercomp02.goal :: [a] -> [Prelude.Int]
supercomp02.goal v1 = supercomp02._pe0 v1

supercomp02.main :: [Prelude.Int]
supercomp02.main = supercomp02.goal (1 : (2 : (3 : [])))

supercomp02._pe0 :: [a] -> [Prelude.Int]
supercomp02._pe0 v1 = 1 : (supercomp02._pe1 v1)

supercomp02._pe1 :: [a] -> [Prelude.Int]
supercomp02._pe1 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> 1 : (supercomp02._pe1 v3)
