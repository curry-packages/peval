PEVAL x = x

[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

goal = PEVAL (let xs free in xs ++ [])

main = null goal
