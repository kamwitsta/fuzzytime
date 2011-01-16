module FuzzyTime.German (showFuzzyTimeDe) where

import {-# SOURCE #-} FuzzyTime

-- German -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeDe :: FuzzyTime -> String
showFuzzyTimeDe ft@(FuzzyTime clock hour _ min night style)
	| min == 0				= if getHour hour `elem` ["Mitternacht", "Mittag"] then getHour hour else getHour hour ++ " Uhr"
	| min `elem` [23..29]	
		&& style == 2		= getMin (30-min) ++ " vor halb " ++ getHour (nextFTHour ft)
	| min < 30				= getMin min ++ " nach " ++ getHour hour
	| min `elem` [31..37]
		&& style == 2		= getMin (min-30) ++ " nach halb " ++ getHour (nextFTHour ft)
	| min == 30				= "halb " ++ getHour (nextFTHour ft)
	| min > 30				= getMin (60-min) ++ " vor " ++ getHour (nextFTHour ft)
	| otherwise	= 			"Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralDe 12 else numeralDe h
								else
								if night then
									if min /=30  then "Mitternacht" else numeralDe clock
								else
									numeralDe 12
		| otherwise			= numeralDe h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "Viertel"
		| otherwise			= numeralDe m


numeralDe :: Int -> String
numeralDe n
	| n < 20			= numeralDeHelper1 n
	| n `mod` 10 == 0	= numeralDeHelper10 (n `div` 10)
	| otherwise			= numeralDeHelper1 (n `mod` 10) ++ "und" ++ numeralDeHelper10 (n `div` 10)
	where
	numeralDeHelper1 :: Int -> String
	numeralDeHelper1 i = ["ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwőlf", "dreizehn", "vierzehn", "fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"] !! (i-1)
	numeralDeHelper10 :: Int -> String
	numeralDeHelper10 i = ["zwanzig", "dreissig", "vierzig", "fünfzig"] !! (i-2)
