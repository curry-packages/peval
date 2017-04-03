module base11 ( base11.id, base11.goal, base11.main ) where

import Prelude

base11.id :: a -> a
base11.id v1 = v1

base11.goal :: a -> a
base11.goal v1 = base11._pe0 v1

base11.main :: Prelude.Int
base11.main = base11.goal 42

base11._pe0 :: a -> a
base11._pe0 v1 = v1
