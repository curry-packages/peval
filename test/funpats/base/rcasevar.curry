rnull xs = case xs of
  []  -> True
  _:_ -> False

goal (rnull xs) = xs

main = (goal True, goal False)
