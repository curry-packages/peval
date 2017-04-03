module const_free ( const_free.PEVAL, const_free.const, const_free.main ) where

import Prelude

const_free.PEVAL :: a -> a
const_free.PEVAL v1 = v1

const_free.const :: a -> b -> a
const_free.const v1 v2 = v1

const_free.main :: Prelude.Int
const_free.main = const_free._pe0

const_free._pe0 :: Prelude.Int
const_free._pe0 = 42
