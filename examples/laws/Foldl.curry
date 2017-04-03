
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

main1 f e y = PEVAL (foldl f e . (y:))

main2 f e y = PEVAL (foldl f (f e y))

