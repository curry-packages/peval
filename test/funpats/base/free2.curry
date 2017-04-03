mkFree _ = x where x free

goal (mkFree x) = x

main = goal True ? goal False ? goal failed
