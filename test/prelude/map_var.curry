PEVAL x = x

inc x = x + 1

map _ []     = []
map f (x:xs) = f x : map f xs

goal xs = PEVAL (map inc xs)

main = goal [1..10]
