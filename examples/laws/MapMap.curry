
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

main1 f g xs = PEVAL (map f (map g xs))

main2 f g xs = PEVAL (map (f . g) xs)
