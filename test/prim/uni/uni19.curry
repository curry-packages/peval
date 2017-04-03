PEVAL x = x

main = PEVAL (let x free in x =:= Just 1 &> x)
