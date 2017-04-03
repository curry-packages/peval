map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

const :: a -> b -> a
const x y = x

goal ys = PEVAL (map (const 1) ys)

main = goal [1..10]
