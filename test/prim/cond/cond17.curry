PEVAL x = x

goal x = PEVAL ((case x of True -> success; False -> failed) &> failed)

main = goal True ? goal False
