import Data.Map

updateValue key new_v old_v = old_v + newValueIfAbsent
newValueIfAbsent = 1

dominantElement [] = Nothing
dominantElement [x] = Just x
dominantElement lst
    | result /= Data.Map.empty = Just (fst (head (Data.Map.toList result)))
    | otherwise = Nothing
        where
          result = Data.Map.filter (> div (length lst) 2) (findDominant lst (Data.Map.fromList []))
          findDominant [] mp = mp
          findDominant [x] mp = insertWithKey updateValue x newValueIfAbsent mp
          findDominant (x:xs) mp = findDominant xs (insertWithKey updateValue x newValueIfAbsent mp)
