-- id x = x
--
-- f (id x) = x
--
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
--
-- last (xs ++ [x]) = x

k0 _ = 0

pair x y = (x, y)

f (pair (k0 x) x) = 0

main = f (0, failed)
