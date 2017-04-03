-- This example requires abstraction!

f x = 1 + f x

main = PEVAL (f 10)
