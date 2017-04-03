import Profile

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

foldr ::  (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

goal xs = PEVAL (foldr (++) [] xs)

main = goal [[1..3], [4..6], [7..10]]

benchmark = do
  let l free
  doSolve (l =:= map (\x -> [x]) [1 .. 200000])
  profileTimeNF (goal l)
