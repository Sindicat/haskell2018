theMostCommonElement :: Eq a => [a] -> Maybe a
theMostCommonElement [] = Nothing
theMostCommonElement [element] = Just element
theMostCommonElement (element:els) = (\(a,b) -> if a > 0 then (topCommonElement (element:els) b) else Nothing) $ topElCommonElement els 1 element

topElCommonElement [element] c symb | c == 0 = (c+1, element)
					   | symb == element = (c+1, symb)
					   | otherwise = (c-1, symb)
topElCommonElement (element:els) c symb | c == 0 = topElCommonElement els (c + 1) element
						  | symb == element = topElCommonElement els (c + 1) symb
						  | otherwise = topElCommonElement els (c - 1) symb

topCommonElement :: Eq a => [a] -> a -> Maybe a
topCommonElement (element:els) n = topCommonElement' (element:els) n ((length (element:els)) `div` 2 + 1) where
		topCommonElement' [element] n len
						| len == 0 = Just n
						| element == n && len <= 1 = Just n
						| otherwise = Nothing

		topCommonElement' (element:els) n len
						| len == 0 = Just n
						| element == n = topCommonElement' els n (len - 1)
						| otherwise = topCommonElement' els n len
