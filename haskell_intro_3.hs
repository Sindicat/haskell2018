sumAndcount::Integer->(Integer,Integer)
sumAndcount x | x < 0 = sumAndcount(-x)
              | x == 0 = (0, 1)
              | otherwise = let
                    helper::Integer->Integer->Integer->(Integer,Integer)
                    helper summ count 0 = (summ, count)
                    helper summ count n = helper (summ + (mod n 10)) (count + 1) (div n 10)
                  in helper 0 0 x
