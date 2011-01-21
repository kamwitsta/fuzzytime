-- Polish -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.Polish (showFuzzyTimePl) where

import {-# SOURCE #-} FuzzyTime


-- showFuzzyTimePl ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimePl :: FuzzyTime -> String


showFuzzyTimePl fc@(FuzzyClock clock hour _ min night style)
	| min == 0			= getHourEven hour
	| min `elem` [23..29]
		&& style == 2	= "za " ++ getMin (30-min) ++ " w pół do " ++ getHourOdd (nextFTHour fc)
	| min < 30			= getMin min ++ " po " ++ getHourOdd hour
	| min == 30			= "w pół do " ++ getHourOdd (nextFTHour fc)
	| min `elem` [31..37]
		&& style == 2	= getMin (min-30) ++ " po w pół do " ++ getHourOdd (nextFTHour fc)
	| min > 30			= "za " ++ getMin (60-min) ++ " " ++ getHourEven (nextFTHour fc)
	| otherwise			= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHourEven :: Int -> String
	getHourEven h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrd "Nom" 12 else numeralPlOrd "Nom" h
								else
								if night then "północ" else numeralPlOrd "Nom" h
		| otherwise			= numeralPlOrd "Nom" h
	getHourOdd :: Int -> String
	getHourOdd h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrd "Praep" 12 else numeralPlOrd "Praep" h
								else
								if night then
									if min < 30 then "północy" else numeralPlOrd "Praep" clock
								else
									numeralPlOrd "Praep" 12
		| otherwise			= numeralPlOrd "Praep" h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwadrans"
		| otherwise			= numeralPlCard m


showFuzzyTimePl (FuzzyTimer _ mins) = "a"


-- numeralPl ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralPlCard :: Int -> String
numeralPlCard n
	| n < 20			= numeralPlCardHelper1 n
	| n `mod` 10 == 0	= numeralPlCardHelper10 (n `div` 10)
	| otherwise			= numeralPlCardHelper10 (n `div` 10) ++ " " ++ numeralPlCardHelper1 (n `mod` 10)
	where
	numeralPlCardHelper1 :: Int -> String
	numeralPlCardHelper1 i = ["jeden", "dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć", "dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "kwadrans", "szesnaście", "siedemnaście", "osiemnaście", "dziewiętnaście"] !! (i-1)
	numeralPlCardHelper10 :: Int -> String
	numeralPlCardHelper10 i = ["dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt"] !! (i-2)


numeralPlOrd :: String -> Int -> String
numeralPlOrd c n
	| n <= 20			= numeralPlOrdHelper1 c n
	| otherwise			= numeralPlOrdHelper10 c ++ " " ++ numeralPlOrdHelper1 c (n `mod` 10)
	where
	numeralPlOrdHelper1 :: String -> Int -> String
	numeralPlOrdHelper1 "Nom" n = ["pierwsza", "druga", "trzecia", "czwarta", "piąta", "szósta", "siódma", "ósma", "dziewiąta", "dziesiąta", "jedenasta", "dwunasta", "trzynasta", "czternasta", "piętnasta", "szesnasta", "siedemnasta", "osiemnasta", "dziewiętnasta", "dwudziesta"] !! (n-1)
	numeralPlOrdHelper1 "Praep" n = ["pierwszej", "drugiej", "trzeciej", "czwartej", "piątej", "szóstej", "siódmej", "ósmej", "dziewiątej", "dziesiątej", "jedenastej", "dwunastej", "trzynastej", "czternastej", "piętnastej", "szesnastej", "siedemnastej", "osiemnastej", "dziewiętnastej", "dwudziestej"] !! (n-1)
	numeralPlOrdHelper10 :: String -> String
	numeralPlOrdHelper10 "Nom" = "dwudziesta"
	numeralPlOrdHelper10 "Praep" = "dwudziestej"

