f x y = fcase x of
  True  -> y
  False -> y

goal x = PEVAL (f x 1)

main = (goal True, goal False)
