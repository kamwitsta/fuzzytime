-- Polish -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module LibFuzzyTime.Polish (showFuzzyTimePl) where

import {-# SOURCE #-} LibFuzzyTime
import Prelude hiding (min)


-- showFuzzyTimePl ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimePl :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimePl fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimePlHlp fc)
showFuzzyTimePl ft@(FuzzyTimer _ _) = showFuzzyTimePlHlp ft

showFuzzyTimePlHlp :: FuzzyTime -> String
showFuzzyTimePlHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0			= getHourEven hour
	| min `elem` [23..29]
		&& style == 2	= "za " ++ getMin (30-min) ++ " w pół do " ++ getHourOdd (nextFTHour fc)
	| min < 30			= getMin min ++ " po " ++ getHourOdd hour
	| min == 30			= "w pół do " ++ getHourOdd (nextFTHour fc)
	| min `elem` [31..37]
		&& style == 2	= getMin (min-30) ++ " po w pół do " ++ getHourOdd (nextFTHour fc)
	| min > 30			= "za " ++ getMin (60-min) ++ " " ++ getHourEven (nextFTHour fc)
	| otherwise			= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHourEven :: Int -> String
	getHourEven h
		| h `elem` [0, 24]	= if style==1 then numeralPlOrd Nom clock else "północ"
		| otherwise			= numeralPlOrd Nom h
	getHourOdd :: Int -> String
	getHourOdd h
		| h `elem` [0, 24]	= if style==1 then
								numeralPlOrd Praep clock
								else
								if min < 30 then "północy" else numeralPlOrd Praep clock
		| otherwise			= numeralPlOrd Praep h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwadrans"
		| otherwise			= numeralPlCard m

-- FuzzyTimer

showFuzzyTimePlHlp (FuzzyTimer _ mins)
	| mins > 0	= "za " ++ showHelper
	| mins == 0	= "teraz!"
	| mins < 0	= "! " ++ showHelper ++ " temu !"
	where
	showHelper :: String
	showHelper
		| mm == 1320	= "dwadzieścia dwie godziny"
		| mm > 1260		= numeralPlCard hours ++ " godziny"
		| mm > 270		= numeralPlCard hours ++ " godzin"
		| mm == 150		= "dwie i pół godziny"
		| mm == 120		= "dwie godziny"
		| mm > 90		= numeralPlCard hours ++ (if half then " i pół" else "") ++ " godziny"
		| mm == 90		= "półtorej godziny"
		| mm == 75		= "godzinę piętnaście"
		| mm == 60		= "godzinę"
		| mm == 45		= "trzy kwadranse"
		| mm == 30		= "pół godziny"
		| mm == 15		= "kwadrans"
		| mm > 4		= numeralPlCard mm ++ " minut"
		| mm > 1		= numeralPlCard mm ++ " minuty"
		| mm == 1		= "minutę"
		| otherwise		= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralPl ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


data Case = Nom | Praep


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


numeralPlOrd :: Case -> Int -> String
numeralPlOrd c num
	| num <= 20			= numeralPlOrdHelper1 c num
	| otherwise			= numeralPlOrdHelper10 c ++ " " ++ numeralPlOrdHelper1 c (num `mod` 10)
	where
	numeralPlOrdHelper1 :: Case -> Int -> String
	numeralPlOrdHelper1 Nom n = ["pierwsza", "druga", "trzecia", "czwarta", "piąta", "szósta", "siódma", "ósma", "dziewiąta", "dziesiąta", "jedenasta", "dwunasta", "trzynasta", "czternasta", "piętnasta", "szesnasta", "siedemnasta", "osiemnasta", "dziewiętnasta", "dwudziesta"] !! (n-1)
	numeralPlOrdHelper1 Praep n = ["pierwszej", "drugiej", "trzeciej", "czwartej", "piątej", "szóstej", "siódmej", "ósmej", "dziewiątej", "dziesiątej", "jedenastej", "dwunastej", "trzynastej", "czternastej", "piętnastej", "szesnastej", "siedemnastej", "osiemnastej", "dziewiętnastej", "dwudziestej"] !! (n-1)
	numeralPlOrdHelper10 :: Case -> String
	numeralPlOrdHelper10 Nom = "dwudziesta"
	numeralPlOrdHelper10 Praep = "dwudziestej"

