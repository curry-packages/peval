map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

const :: a -> b -> a
const x y = x

goal ys = PEVAL (let x = 1; xs = map (const 1) ys in x:xs)

main = goal [1,2,3]
