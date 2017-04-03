module base09 ( base09.bind, base09.goal, base09.main ) where

import Prelude

base09.bind :: () -> Prelude.Bool
base09.bind v1 = v1 Prelude.=:= ()

base09.goal :: Prelude.Bool -> ()
base09.goal v1 = base09._pe0 v1

base09.main :: ()
base09.main = base09.goal Prelude.success

base09._pe0 :: Prelude.Bool -> ()
base09._pe0 v1 = fcase v1 of
    Prelude.True -> ()
