import Data.Maybe

minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax [element] = Just (element, element)
minMax (element:els) = Just $ findMinAndMax element $ minMax els where
  findMinAndMax element (Just(min,max))
    | element > max = (min, element)
    | element < min = (element, max)
    | otherwise = (min, max)
