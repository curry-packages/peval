PEVAL x = x

flip f x y = f y x

main = PEVAL (flip (-) 1 43)
