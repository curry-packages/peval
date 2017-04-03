PEVAL x = x

id x = x

filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

main = PEVAL (filter id [True, False, False, True])
