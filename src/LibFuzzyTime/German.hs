-- German (thanks Clad in the sky, ichbinsisyphos and marens from forums.gentoo.org) ----------------------------------------------------------------------------------------------


module LibFuzzyTime.German (showFuzzyTimeDe) where

import {-# SOURCE #-} LibFuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeDe ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeDe :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeDe fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeDeHlp fc)
showFuzzyTimeDe ft@(FuzzyTimer _ _) = showFuzzyTimeDeHlp ft

showFuzzyTimeDeHlp :: FuzzyTime -> String
showFuzzyTimeDeHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0				= if getHour hour `elem` ["Mitternacht", "Mittag"] then getHour hour else getHour hour ++ " Uhr"
	| min == 15
		&& style == 3		= "Viertel " ++ getHour (nextFTHour fc)
	| min `elem` [23..29]	
		&& style >= 2		= getMin (30-min) ++ " vor halb " ++ getHour (nextFTHour fc)
	| min < 30				= getMin min ++ " nach " ++ getHour hour
	| min `elem` [31..37]
		&& style >= 2		= getMin (min-30) ++ " nach halb " ++ getHour (nextFTHour fc)
	| min == 30				= "halb " ++ getHour (nextFTHour fc)
	| min == 45
		&& style == 3		= "Dreiviertel " ++ getHour (nextFTHour fc)
	| min > 30				= getMin (60-min) ++ " vor " ++ getHour (nextFTHour fc)
	| otherwise				= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then
								numeralDe clock
								else
								if min /=30 then "Mitternacht" else numeralDe clock
		| otherwise			= numeralDe h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "Viertel"
		| otherwise			= numeralDe m

-- FuzzyTimer

showFuzzyTimeDeHlp (FuzzyTimer _ mins)
	| mins > 0	= "in " ++ showHelper
	| mins == 0	= "jetzt!"
	| mins < 0	= "! vor " ++ showHelper ++ " !"
	where
	showHelper :: String
	showHelper
		| mm >= 90	= numeralDe hours ++ (if half then "einhalb" else "") ++ " Stunden"
		| mm == 75	= "einer Stunde und fünfzehn Minuten"
		| mm == 60	= "einer Stunde"
		| mm == 45	= "einer Dreiviertelstunde"
		| mm == 30	= "eine halbe Stunde"
		| mm == 15	= "einer ViertelStunde"
		| mm > 1	= numeralDe mm ++ " Minuten"
		| mm == 1	= "einer Minute"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralDe ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


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
