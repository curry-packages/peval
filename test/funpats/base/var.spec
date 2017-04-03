module var ( var.id, var.goal, var.main ) where

import Prelude

var.id :: a -> a
var.id v1 = v1

var.goal :: a -> a
var.goal v1 = var._pe0 v1

var.main :: Prelude.Int
var.main = var.goal 1

var._pe0 :: a -> a
var._pe0 v1 = v1
