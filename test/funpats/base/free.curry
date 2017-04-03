mkFree _ = x where x free

goal (mkFree 0) = 42

main = (goal True, goal failed)
