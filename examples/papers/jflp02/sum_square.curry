import Debug.Profile

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

foldr ::  (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

square :: Int -> Int
square x = x * x

-- -----------------------------------------------------------------------------
-- SUM OF SQUARES
-- -----------------------------------------------------------------------------

goal xs = PEVAL (foldr (+) 0 (map square xs))

main = goal [1..10]

benchmark = do
  let l free
  doSolve (l =:= [1 .. 200000])
  profileTimeNF (goal l)
