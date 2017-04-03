coin _ = True
coin _ = False

goal (coin 0) = 0

main = goal True ? goal False
