
id x = x

mkPair x y = (id x, id y)

nonLinear (mkPair x x) = x + x

main = nonLinear (1,1) ? nonLinear (1,2)
