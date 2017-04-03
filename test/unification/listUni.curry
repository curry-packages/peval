[]       =:= []      = success
(():xs)  =:= (():ys) = xs =:= ys

check zs | xs =:= zs = xs where xs free

goal zs = PEVAL (check zs)

main = goal (replicate 3 ())
