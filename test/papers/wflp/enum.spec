module enum ( enum.enumFT, enum.goal, enum.main ) where

import Prelude

enum.enumFT :: Prelude.Int -> Prelude.Int -> [Prelude.Int]
enum.enumFT v1 v2 = case (v1 > v2) of
    Prelude.True -> []
    Prelude.False -> v1 : (enum.enumFT (v1 + 1) v2)

enum.goal :: Prelude.Int -> [Prelude.Int]
enum.goal v1 = enum._pe0 v1

enum.main :: [Prelude.Int]
enum.main = enum.goal 10

enum._pe0 :: Prelude.Int -> [Prelude.Int]
enum._pe0 v1 = case (1 > v1) of
    Prelude.True -> []
    Prelude.False -> 1 : (enum.enumFT 2 v1)
