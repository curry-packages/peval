PEVAL x = x

take _ []     = []
take n (x:xs) = if n <= 0 then [] else x : take (n - 1) xs

main = PEVAL (take 1 [1,2,3])
