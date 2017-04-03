PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal = PEVAL (let xs, x free in (xs ++ [x]))

main = null goal
