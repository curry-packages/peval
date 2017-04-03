PEVAL x = x

inc x = x + 1

map _ []     = []
map f (x:xs) = f x : map f xs

main = PEVAL (map inc [1,2,3])
