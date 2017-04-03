id x = x

goal (id [x, True]) = x

main = goal [True, True] ? goal [] ? goal [False, True]
