import Profile

data Nat = Z | S Nat

allones Z     = []
allones (S x) = 1 : allones x

length []       = Z
length (_ : xs) = S (length xs)

goal xs = PEVAL (allones (length xs))

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 200000])
  profileTimeNF (goal l)
