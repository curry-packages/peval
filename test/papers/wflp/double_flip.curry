import Profile

data TreeInt = Leaf Int | Tree Int TreeInt TreeInt

-- generate a treee of depth n:
gentree n | n == 0    = Leaf 0
          | otherwise = Tree n (gentree (n-1)) (gentree (n-1))

fliptree (Leaf     a) = Leaf a
fliptree (Tree x l r) = Tree x (fliptree r) (fliptree l)

goal t = PEVAL (fliptree (fliptree t))

main = goal t == t where t = gentree 5

benchmark = do
  let t free
  doSolve (t =:= gentree 17)
  profileTimeNF (goal t)
