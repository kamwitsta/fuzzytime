-- Norwegian (Bokmål) (thanks arnvidr from forums.gentoo.org) ---------------------------------------------------------------------------------------------------------------------


module FuzzyTime.Norwegian (showFuzzyTimeNb) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeNb ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeNb :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeNb fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeNbHlp fc)
showFuzzyTimeNb ft@(FuzzyTimer _ _) = showFuzzyTimeNbHlp ft

showFuzzyTimeNbHlp :: FuzzyTime -> String
showFuzzyTimeNbHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0				= getHour hour
	| min `elem` [20..29]	= getMin (30-min) ++ " på halv " ++ getHour (nextFTHour fc)
	| min < 30				= getMin min ++ " over " ++ getHour hour
	| min `elem` [31..40]	= getMin (min-30) ++ " over halv " ++ getHour (nextFTHour fc)
	| min == 30				= "halv " ++ getHour (nextFTHour fc)
	| min > 30				= getMin (60-min) ++ " på " ++ getHour (nextFTHour fc)
	| otherwise				= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then numeralNb clock else "midnatt"
		| otherwise			= numeralNb h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kvart"
		| otherwise 		= numeralNb m

-- FuzzyTimer

showFuzzyTimeNbHlp (FuzzyTimer _ mins)
	| mins > 0	= "om " ++ showHelper
	| mins == 0	= "nå!"
	| mins < 0	= "! for " ++ showHelper ++ " siden !"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralNb hours ++ (if half then " og en halv" else "") ++ " time"
		| mm == 90	= "halvannen time"
		| mm == 75	= "en time og ett kvarter"
		| mm == 60	= "en time"
		| mm == 45	= "tre kvarter"
		| mm == 30	= "halv time"
		| mm == 15	= "en kvarter"
		| mm > 1	= numeralNb mm ++ " minutter"
		| mm == 1	= "en minutt"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralDa ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralNb :: Int -> String
numeralNb n
	| n < 20			= numeralNbHelper1 n
	| n `mod` 10 == 0	= numeralNbHelper10 (n `div` 10)
	| otherwise			= numeralNbHelper10 (n `div` 10) ++ " " ++ numeralNbHelper1 (n `mod` 10)
	where
	numeralNbHelper1 :: Int -> String
	numeralNbHelper1 i = ["en", "to", "tre", "fire", "fem", "seks", "sju", "åtte", "ni", "ti", "elleve", "tolv", "tretten", "fjorten", "femten", "seksten", "sytten", "atten", "nitten"] !! (i-1)
	numeralNbHelper10 :: Int -> String
	numeralNbHelper10 i = ["tjue", "tretti", "førti", "femti"] !! (i-2)
