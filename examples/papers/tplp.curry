-- This example has been taken from:
--   Mar{\'{\i}}a Alpuente and Salvador Lucas and Michael Hanus and Germ{\'{a}}n Vidal
--   Specialization of Functional Logic Programs Based on Needed Narrowing
--   TPLP'05

-- Example 7

data N = O | S N

PEVAL x = x

f O = O

g x = S (f x)

h (S x) = S O

main = h (PEVAL (g (S O)))
