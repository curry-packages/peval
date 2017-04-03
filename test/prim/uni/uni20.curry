goal xs = PEVAL (let x free in [x] =:= xs)

main = goal [1] ? goal [1,2]
