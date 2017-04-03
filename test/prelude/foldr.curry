PEVAL x = x

foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

main = PEVAL (foldr (+) 0 [1,2,3])
