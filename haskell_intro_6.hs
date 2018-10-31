customFib n = (standardFib n) `mod` 10

standardFib n = counterFib n 0 1

counterFib n a b | n == 0 = a
						     | otherwise = counterFib (n-1) (a + b) a
