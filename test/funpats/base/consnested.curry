id x = x

goal (id [True]) = 42

main = goal [True] ? goal [] ? goal [False]
