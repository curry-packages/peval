PEVAL x = x

goal x = PEVAL (x =:= 1)

main = goal x &> x where x free
