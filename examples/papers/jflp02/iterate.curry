import Profile

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

comp :: (a -> b) -> (c -> a) -> c -> b
comp f g x = f (g x)

inc :: Int -> Int
inc x = x + 1

-- beyond higher-order macros:
iter f n = if n == 0 then f else iter (comp f f) (n - 1)

goal xs = PEVAL (map (iter inc 2) xs)

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 200000])
  profileTimeNF (goal l)
