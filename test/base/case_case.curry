PEVAL x = x

data ID a = ID a

id False = ID False
id True  = ID True

goal x = PEVAL (case id x of ID True -> True; ID False -> False)

main = (goal True, goal False)
