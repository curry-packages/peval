import Profile
import Unsafe

import Control.SetFunctions

foldr ::  (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

goal xs = PEVAL (foldr (?) failed xs)

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 10000])
  profileTimeNF $ unsafePerformIO $ values2list $ set1 goal l
