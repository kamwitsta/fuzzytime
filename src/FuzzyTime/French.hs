-- French -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.French (showFuzzyTimeFr) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeFr ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeFr :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeFr fc@(FuzzyClock _ caps clock hour _ min style)
	| min == 0	= capsizeDef caps $ getHour hour
	| min <= 30	= capsizeDef caps $ getHour hour ++ " " ++ getMin min
	| min > 30	= capsizeDef caps $ getHour (nextFTHour fc) ++ " moins " ++ getMin (60-min)
	| otherwise	= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then numeralFr clock ++ getHourWord clock else "minuit"
		| h == 12			= if style==1 then numeralFr 12 else "midi"
		| otherwise			= numeralFr h ++ getHourWord h
	getHourWord :: Int -> String
	getHourWord h = if h==1 then " heure" else " heures"
	getMin :: Int -> String
	getMin m
		| min == 15			= "et quart"
		| min == 30			= "et demie"
		| min == 45			= "le quart"
		| otherwise			= numeralFr m

-- FuzzyTimer

showFuzzyTimeFr (FuzzyTimer _ mins)
	| mins > 0	= "dans " ++ showHelper
	| mins == 0	= "maintenant!"
	| mins < 0	= "! il y a " ++ showHelper ++ " !"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralFr hours ++ " heures " ++ (if half then " et demie" else "")
		| mm == 90	= "une heure et demie"
		| mm == 75	= "une heure et quart"
		| mm == 60	= "une heure"
		| mm == 45	= "trois quarts d’heure"
		| mm == 30	= "une demie d’heure"
		| mm == 15	= "un quart d’heure"
		| mm > 1	= numeralFr mm ++ " minutes"
		| mm == 1	= "une minute"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


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

