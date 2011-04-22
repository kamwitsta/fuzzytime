-- Dutch (thanks Boris from forums.gentoo.org and litemotiv from bbs.archlinux.org) -----------------------------------------------------------------------------------------------


module FuzzyTime.Dutch (showFuzzyTimeNl) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeNl ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeNl :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeNl fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeNlHlp fc)
showFuzzyTimeNl ft@(FuzzyTimer _ _) = showFuzzyTimeNlHlp ft

showFuzzyTimeNlHlp :: FuzzyTime -> String
showFuzzyTimeNlHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0				= if getHour hour == "middernacht" then getHour hour else getHour hour ++ " uur"
	| min `elem` [20..29]
		&& style == 2		= getMin (30-min) ++ " voor half " ++ getHour (nextFTHour fc)
	| min < 30				= getMin min ++ " over " ++ getHour hour
	| min `elem` [31..40]
		&& style == 2		= getMin (min-30) ++ " over half " ++ getHour (nextFTHour fc)
	| min == 30				= "half " ++ getHour (nextFTHour fc)
	| min > 30				= getMin (60-min) ++ " voor " ++ getHour (nextFTHour fc)
	| otherwise	=			"Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then
								numeralNl clock
								else
								if min /= 30 then "middernacht" else numeralNl clock
		| otherwise			= numeralNl h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwart"
		| otherwise			= numeralNl m

-- FuzzyTimer

showFuzzyTimeNlHlp (FuzzyTimer _ mins)
	| mins > 0	= "over " ++ showHelper
	| mins == 0	= "nu!"
	| mins < 0	= "! " ++ showHelper ++ " geleden !"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralNl hours ++ (if half then " en een half" else "") ++ " uur"
		| mm == 90	= "anderhalf uur"
		| mm == 75	= "vijf kwartier"
		| mm == 60	= "een uur"
		| mm == 45	= "drie kwartier"
		| mm == 30	= "een half uur"
		| mm == 15	= "een kwartier"
		| mm > 1	= numeralNl mm ++ " minuten"
		| mm == 1	= "een minuut"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralNl ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralNl :: Int -> String
numeralNl n
	| n < 20			= numeralNlHelper1 n
	| n `mod` 10 == 0	= numeralNlHelper10 (n `div` 10)
	| otherwise			= numeralNlHelper1 (n `mod` 10) ++
							(if (n `mod` 10) `elem` [2, 3] then "ën" else "en") ++
							numeralNlHelper10 (n `div` 10)
	where
	numeralNlHelper1 :: Int -> String
	numeralNlHelper1 i = ["een", "twee", "drie", "vier", "vijf", "zes", "zeven", "acht", "negen", "tien", "elf", "twaalf", "dertien", "veertien", "vijftien", "zestien", "zeventien", "achttien", "negentien"] !! (i-1)
	numeralNlHelper10 :: Int -> String
	numeralNlHelper10 i = ["twintig", "dertig", "veertig", "vijftig"] !! (i-2)
