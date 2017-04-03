-- Example 5 of LPAR'99 paper:

data ABC = A | B | C

-- f eval flex
-- f A B = C
f a b = fcase a of
  A -> fcase b of
    B -> C

-- g eval rigid
-- g B C = B
g b c = case b of
  B -> case c of
    C -> B

-- h eval flex
-- h C = C
h c = fcase c of C -> C

goal x y z = PEVAL (f x (g y (h z)))

main = (goal x y z, x, y, z) where x, y, z free
