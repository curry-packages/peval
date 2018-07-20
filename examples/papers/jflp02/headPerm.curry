import Debug.Profile
import SetFunctions
import System.IO.Unsafe

perm []     = []
perm (x:xs) = insert x (perm xs)

insert x ys     = x : ys
insert x (y:ys) = y : insert x ys

head (x:_) = x

goal xs = PEVAL (head (perm xs))

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 10000])
  profileTimeNF $ unsafePerformIO $ values2list $ set1 goal l
