-- Turkish ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

module FuzzyTime.Turkish (showFuzzyTimeTr) where

import {-# SOURCE #-} FuzzyTime


showFuzzyTimeTr :: FuzzyTime -> String
showFuzzyTimeTr ft@(FuzzyTime clock hour _ min night style)
	| min == 0			= "saat " ++ getHour "Nom" hour
	| min `elem` [20..29]
		&& style == 2	= getHour "Nom" hour ++ " buçuğa " ++ getMin (30-min) ++ " var"
	| min < 30			= getHour "Acc" hour ++ " " ++ getMin min ++ " geçiyor"
	| min == 30			= if hour `mod` 12 == 0 then "saat yarım" else getHour "Nom" hour ++ " buçuk"
	| min `elem` [31..40]
		&& style == 2	= getHour "Nom" hour ++ " buçuğu " ++ getMin (min-30) ++ " geçiyor"
	| min > 30			= getHour "Dat" (nextFTHour ft) ++ " " ++ getMin (60-min) ++ " var"
	| otherwise			= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: String -> Int -> String
	getHour c h
		| h `mod` 12 == 0	= if clock==12 then numeralTr c 12 else numeralTr c h
		| otherwise			= numeralTr c h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "çeyrek"
		| otherwise			= numeralTr "Nom" m


numeralTr :: String -> Int -> String
numeralTr c n
	| n < 10			= numeralTrHelper1 c n
	| n `mod` 10 == 0	= numeralTrHelper10 c (n `div` 10)
	| otherwise			= numeralTrHelper10 "Nom" (n `div` 10) ++ " " ++ numeralTrHelper1 c (n `mod` 10)
	where
	numeralTrHelper1 :: String -> Int -> String
	numeralTrHelper1 "Nom" i = ["bir", "iki", "üç", "dört", "beş", "altı", "yedi", "sekiz", "dokuz"] !! (i-1)
	numeralTrHelper1 "Dat" i = ["bire", "ikiye", "üçe", "dörde", "beşe", "altıya", "yediye", "sekize", "dokuza"] !! (i-1)
	numeralTrHelper1 "Acc" i = ["biri", "ikiyi", "üçü", "dördü", "beşi", "altıyı", "yediyi", "sekizi", "dokuzu"] !! (i-1)
	numeralTrHelper10 :: String -> Int -> String
	numeralTrHelper10 "Nom" i = ["on", "yirmi", "otuz", "kırk", "elli"] !! (i-1)
	numeralTrHelper10 "Dat" i = ["ona", "yirmiye", "otuza", "kırka", "elliye"] !! (i-1)
	numeralTrHelper10 "Acc" i = ["onu", "yirmiyi", "otuzu", "kırkı", "elliyi"] !! (i-1)
