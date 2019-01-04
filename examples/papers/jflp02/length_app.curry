import Debug.Profile

data Nat = Z | S Nat

length2 [] = Z
length2 (x:xs) = S (length2 xs)

append [] ys = ys
append (x:xs) ys = x:(append xs ys)

goal xs ys = PEVAL (length2 (append xs ys))

main = goal [1..10] [11..20]

benchmark = do
  let l1,l2 free
  doSolve (l1 =:= [1..200000] &> l2 =:= [1..1000])
  profileTimeNF (goal l1 l2)
