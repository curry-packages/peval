ones :: [Int]
ones = 1 : ones

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

main = PEVAL (map (\x -> x + 1) ones)
