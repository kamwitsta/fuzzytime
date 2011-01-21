-- French -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.French (showFuzzyTimeFr) where

import {-# SOURCE #-} FuzzyTime


-- showFuzzyTimeFr ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeFr :: FuzzyTime -> String


showFuzzyTimeFr fc@(FuzzyClock clock hour _ min night style)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " " ++ getMin min
	| min > 30	= getHour (nextFTHour fc) ++ " moins " ++ getMin (60-min)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								(if clock==12 then numeralFr 12 else numeralFr h) ++ getHourWord h
								else
								if night then "minuit" else "midi"
		| otherwise			= numeralFr h ++ getHourWord h
	getHourWord :: Int -> String
	getHourWord h = if h==1 then " heure" else " heures"
	getMin :: Int -> String
	getMin m
		| min == 15			= "et quart"
		| min == 30			= "et demie"
		| min == 45			= "le quart"
		| otherwise			= numeralFr m


showFuzzyTimeFr (FuzzyTimer _ mins) = "French is not supported in the timer mode."


-- numeralFr ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralFr :: Int -> String
numeralFr n
	| n < 20			= numeralFrHelper1 n
	| n `mod` 10 == 0	= numeralFrHelper10 (n `div` 10)
	| otherwise			= numeralFrHelper10 (n `div` 10) ++ "-" ++ numeralFrHelper1 (n `mod` 10)
	where
	numeralFrHelper1 :: Int -> String
	numeralFrHelper1 i = ["une", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf"] !! (i-1)
	numeralFrHelper10 :: Int -> String
	numeralFrHelper10 i = ["vingt", "trente", "quarante", "cinquante"] !! (i-2)

