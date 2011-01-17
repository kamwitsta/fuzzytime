-- English ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

module FuzzyTime.English (showFuzzyTimeEn) where

import {-# SOURCE #-} FuzzyTime


showFuzzyTimeEn :: FuzzyTime -> String
showFuzzyTimeEn ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= if getHour hour `elem` ["midnight", "noon"] then getHour hour else getHour hour ++ " o’clock"
	| min <= 30	= getMin min ++ " past " ++ getHour hour
	| min > 30	= getMin (60-min) ++ " to " ++ getHour (nextFTHour ft)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralEn 12 else numeralEn h
								else
								if night then "midnight" else "noon"
		| otherwise			= numeralEn h
	getMin :: Int -> String
	getMin m
		| m == 30			= "half"
		| m `elem` [15, 45]	= "quarter"
		| otherwise			= numeralEn m


numeralEn :: Int -> String
numeralEn n
	| n < 20			= numeralEnHelper1 n
	| n `mod` 10 == 0	= numeralEnHelper10 (n `div` 10)
	| otherwise			= numeralEnHelper10 (n `div` 10) ++ "-" ++ numeralEnHelper1 (n `mod` 10)
	where
	numeralEnHelper1 :: Int -> String
	numeralEnHelper1 i = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! (i-1)
	numeralEnHelper10 :: Int -> String
	numeralEnHelper10 i = ["twenty", "thirty", "forty", "fifty"] !! (i-2)
