import Profile

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

square :: Int -> Int
square x = x * x

goal x = PEVAL (map (twice square) x)

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 200000])
  profileTimeNF (goal l)
