square  x = x * x
even    x = mod x 2 == 0
power n x = if n <= 0 then 1
                      else if (even n) then power (div n 2) (square x)
                                       else x * (power (n - 1) x)
power4  x = PEVAL (power 4 x)
