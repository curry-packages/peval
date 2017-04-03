data N = O | S N

PEVAL x = x

f O = O

g x = S (f x)

h (S x) = S O

main = h (PEVAL (g (S O)))