import External (e1)

sumList []     = 0
sumList (x:xs) = x + sumList xs

incList _ []     = []
incList n (x:xs) = (x + n) : incList n xs

e = e1 + 42

goal a b = PEVAL (sumList (incList e [a, b]))

main = goal 1 2
