import Profile

foldr ::  (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

goal xs = PEVAL (foldr (+) 0 xs)

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 200000])
  profileTimeNF (goal l)
