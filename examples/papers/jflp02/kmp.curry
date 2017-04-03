import Profile

data Letter = A | B

match p s = loop p s p s

loop []     _      _  _  = True
loop (_:_)  []     _  _  = False
loop (p:ps) (s:ss) op os = if p `eq` s then loop ps ss op os else next op os
-- loop (p:ps) (s:ss) op os = ite (eq p s) (loop ps ss op os) (next op os)

next _  [] = False
next op (_:ss) = loop op ss op ss

ite True  x _ = x
ite False _ y = y

eq A A = True
eq B B = True
eq A B = False
eq B A = False

goal s = PEVAL (match [A,A,B] s)
-- goal s = PEVAL (match [A,A,B] s)

main = map goal [[A,A,A,B], [A,A,B], [A,A], [B]]

benchmark = do
  let l free
  doSolve (l =:= take 200000 (repeat A) ++ [B])
  profileTimeNF (goal l)
