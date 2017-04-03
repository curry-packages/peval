
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs
                         else     filter p xs

goal xs = PEVAL (filter (flip (>) 100) (map (flip (*) 3) xs))

main = goal [10, 20 .. 200]

benchmark = let l,r free in l =:= [1..20000] &>
                            goal l =:= r
