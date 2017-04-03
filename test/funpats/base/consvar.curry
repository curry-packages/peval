id x = x

goal (id [x]) = x

main = goal [True] ? goal [] ? goal [False]
