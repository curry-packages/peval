module infiniteBindings ( infiniteBindings.f, infiniteBindings.main ) where

import Prelude

infiniteBindings.f :: [a] -> b
infiniteBindings.f v1 = let v2 free in infiniteBindings.f (v2 : v1)

infiniteBindings.main :: a
infiniteBindings.main = infiniteBindings._pe0

infiniteBindings._pe0 :: a
infiniteBindings._pe0 = infiniteBindings._pe1 ((let v1 free in v1) : [])

infiniteBindings._pe1 :: [a] -> b
infiniteBindings._pe1 v1 = infiniteBindings._pe1 ((let v2 free in v2) : v1)
