PEVAL x = x

goal x = PEVAL (1 =:= x)

main = goal x &> x where x free
