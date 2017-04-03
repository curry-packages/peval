PEVAL x = x

foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

main = PEVAL (foldl (+) 0 [1,2,3])
