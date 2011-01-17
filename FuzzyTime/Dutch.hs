-- Dutch (thanks litemotiv from bbs.archlinux.org)---------------------------------------------------------------------------------------------------------------------------------

module FuzzyTime.Dutch (showFuzzyTimeNl) where

import {-# SOURCE #-} FuzzyTime


showFuzzyTimeNl :: FuzzyTime -> String
showFuzzyTimeNl ft@(FuzzyTime clock hour _ min night style)
	| min == 0				= if getHour hour == "middernacht" then getHour hour else getHour hour ++ " uur"
	| min `elem` [20..29]	
		&& style == 2		= getMin (30-min) ++ " voor half " ++ getHour (nextFTHour ft)
	| min < 30				= getMin min ++ " over " ++ getHour hour
	| min `elem` [31..40]
		&& style == 2		= getMin (min-30) ++ " over half " ++ getHour (nextFTHour ft)
	| min == 30				= "half " ++ getHour (nextFTHour ft)
	| min > 30				= getMin (60-min) ++ " voor " ++ getHour (nextFTHour ft)
	| otherwise	= 			"Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralNl 12 else numeralNl h
								else
								if night then
									if min /= 30 then "middernacht" else numeralNl clock
								else
									numeralNl 12
		| otherwise			= numeralNl h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwart"
		| otherwise			= numeralNl m


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
