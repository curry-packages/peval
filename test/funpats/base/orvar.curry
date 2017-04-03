coin :: Bool -> Bool
coin _ = True
coin _ = False

goal (coin x) = x

main = goal True ? goal False
