f xs = let x free in f (x:xs)

main = PEVAL (f [])
