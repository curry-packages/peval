module var2 ( var2.id, var2.goal, var2.main ) where

import Prelude

var2.id :: a -> a
var2.id v1 = v1

var2.goal :: a -> Prelude.Int
var2.goal v1 = var2._pe0 v1

var2.main :: Prelude.Int
var2.main = var2.goal 1

var2._pe0 :: a -> Prelude.Int
var2._pe0 v1 = 0
