import Data.Char

isPalindrome :: String -> Bool
isPalindrome string = check (map toUpper string) True where
	check string boolean | not boolean || length string <= 1 = boolean
				               | otherwise = check (substring string) ((head string) == (last string))
substring :: String -> String
substring string = tail $ take ((length string)-1) string
