not False = True
not True  = False

main x = PEVAL (not (fcase x of True -> False; False -> True))
