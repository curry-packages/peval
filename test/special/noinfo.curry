-- map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x + (1 + 1) : map f xs

goal f xs = PEVAL (map f xs)

main = goal (+1) [1..10]
