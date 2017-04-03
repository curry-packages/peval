null []    = True
null (_:_) = False

goal (null xs) = xs

main = (goal True, goal False)
