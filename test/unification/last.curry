
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

last zs | xs ++ [x] =:= zs = x where xs, x free

goal zs = PEVAL (last zs)

main = goal [1..10] ? goal [failed, 1]
