module dsl11
  ( dsl11.State, dsl11.Label, dsl11.Trans, dsl11.Accept, dsl11.transitions
  , dsl11.accept, dsl11.elem, dsl11.lookup, dsl11.run, dsl11.goal, dsl11.main )
  where

import Prelude

type dsl11.State = Prelude.Int

type dsl11.Label = Prelude.Char

type dsl11.Trans = [(Prelude.Int,[(Prelude.Char,Prelude.Int)])]

type dsl11.Accept = [Prelude.Int]

dsl11.transitions :: [(Prelude.Int,[(Prelude.Char,Prelude.Int)])]
dsl11.transitions = (1,('a',2) : []) : ((2,('b',1) : []) : [])

dsl11.accept :: [Prelude.Int]
dsl11.accept = 2 : []

dsl11.elem :: a -> [a] -> Prelude.Bool
dsl11.elem v1 v2 = fcase v2 of
    [] -> Prelude.False
    v3 : v4 -> case (v1 == v3) of
        Prelude.True -> Prelude.True
        Prelude.False -> dsl11.elem v1 v4

dsl11.lookup :: a -> [(a,b)] -> Prelude.Maybe b
dsl11.lookup v1 v2 = fcase v2 of
    [] -> Prelude.Nothing
    v3 : v4 -> fcase v3 of
        (v5,v6) -> case (v1 == v5) of
            Prelude.True -> Prelude.Just v6
            Prelude.False -> dsl11.lookup v1 v4

dsl11.run
  ::
  Prelude.Int -> [Prelude.Int] -> [(Prelude.Int,[(Prelude.Char,Prelude.Int)])]
  -> [Prelude.Char] -> Prelude.Bool
dsl11.run v1 v2 v3 v4 = fcase v4 of
    [] -> dsl11.elem v1 v2
    v5 : v6 -> case (dsl11.lookup v1 v3) of
        Prelude.Nothing -> Prelude.False
        Prelude.Just v7 -> case (dsl11.lookup v5 v7) of
            Prelude.Nothing -> Prelude.False
            Prelude.Just v8 -> dsl11.run v8 v2 v3 v6

dsl11.goal :: [Prelude.Char] -> Prelude.Bool
dsl11.goal v1 = dsl11._pe0 v1

dsl11.main :: [Prelude.Bool]
dsl11.main = Prelude.map
  dsl11.goal
  (('a' : [])
  :
  (('a' : ('b' : ('a' : [])))
  :
  (('a' : ('b' : ('a' : ('b' : ('a' : []))))) : (('b' : []) : ([] : [])))))

dsl11._pe0 :: [Prelude.Char] -> Prelude.Bool
dsl11._pe0 v1 = fcase v1 of
    [] -> Prelude.False
    v2 : v3 -> case (v2 == 'a') of
        Prelude.True -> fcase v3 of
            [] -> Prelude.True
            v4 : v5 -> case (v4 == 'b') of
                Prelude.True -> dsl11._pe0 v5
                Prelude.False -> Prelude.False
        Prelude.False -> Prelude.False
