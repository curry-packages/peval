
f True = True

g x = (f x, f x)

main = PEVAL (g (False ? True))