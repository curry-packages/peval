module let_sharing
  ( let_sharing.PEVAL, let_sharing.goal, let_sharing.main )
  where

import Prelude

let_sharing.PEVAL :: a -> a
let_sharing.PEVAL v1 = v1

let_sharing.goal :: [Prelude.Int]
let_sharing.goal = let_sharing._pe0

let_sharing.main :: [Prelude.Int]
let_sharing.main = Prelude.take 10 let_sharing.goal

let_sharing._pe0 :: [Prelude.Int]
let_sharing._pe0 = let { v1 = 0 ? 1 } in v1 : (let_sharing._pe1 v1)

let_sharing._pe1 :: a -> [a]
let_sharing._pe1 v1 = v1 : (let_sharing._pe1 v1)
