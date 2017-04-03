
[]       =:= []      = success
(():xs)  =:= (():ys) = xs =:= ys

goal x2 x3 = PEVAL (let x1 free in cond (x1 =:= x2) x3)

main = goal (replicate 3 ()) True
