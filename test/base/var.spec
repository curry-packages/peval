module var ( var.PEVAL, var.goal, var.main ) where

import Prelude

var.PEVAL :: a -> a
var.PEVAL v1 = v1

var.goal :: a -> a
var.goal v1 = v1

var.main :: Prelude.Bool
var.main = var.goal Prelude.True
