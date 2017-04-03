PEVAL x = x

f Nothing  = Nothing
f (Just x) = Just (x + x)

goal x = PEVAL (f x)

main = (goal Nothing, goal (Just 1))
