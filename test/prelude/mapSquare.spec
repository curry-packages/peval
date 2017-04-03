module mapSquare
  ( mapSquare.map, mapSquare.square, mapSquare.goal, mapSquare.main
  , mapSquare.benchmark )
  where

import Prelude
import Profile

mapSquare.map :: (a -> b) -> [a] -> [b]
mapSquare.map v1 v2 = fcase v2 of
    [] -> []
    v3 : v4 -> (Prelude.apply v1 v3) : (mapSquare.map v1 v4)

mapSquare.square :: Prelude.Int -> Prelude.Int
mapSquare.square v1 = v1 * v1

mapSquare.goal :: [Prelude.Int] -> [Prelude.Int]
mapSquare.goal v1 = mapSquare._pe0 v1

mapSquare.main :: [Prelude.Int]
mapSquare.main = mapSquare.goal (Prelude.enumFromTo 1 10)

mapSquare.benchmark :: Prelude.IO ()
mapSquare.benchmark = let v1 free
  in (Prelude.doSolve (v1 Prelude.=:= (Prelude.enumFromTo 1 500000)))
  Prelude.>>
  (Profile.profileTimeNF (mapSquare.goal v1))

mapSquare._pe0 :: [Prelude.Int] -> [Prelude.Int]
mapSquare._pe0 v1 = fcase v1 of
    [] -> []
    v2 : v3 -> (v2 * v2) : (mapSquare._pe0 v3)
