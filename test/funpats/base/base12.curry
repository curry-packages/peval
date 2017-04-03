
pair x y = (x, y)

match x = fcase x of True -> True

goal (pair x (match x)) = x

main = goal (True, True) ? goal (False, True)
