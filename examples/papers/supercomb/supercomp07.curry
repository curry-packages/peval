goal x = PEVAL (let y = case x of True -> False in case y of False -> (2, y))

main = goal True ? goal False
