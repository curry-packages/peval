PEVAL x = x

True  && x = x
False && _ = False

goal x = PEVAL (case x of True -> failed; False -> failed)

main = goal True ? goal False
