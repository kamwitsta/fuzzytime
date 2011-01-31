-- English ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.English (showFuzzyTimeEn) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeEn ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeEn :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeEn fc@(FuzzyClock _ clock hour _ min style)
	| min == 0	= if getHour hour `elem` ["midnight", "noon"] then getHour hour else getHour hour ++ " o’clock"
	| min <= 30	= getMin min ++ " past " ++ getHour hour
	| min > 30	= getMin (60-min) ++ " to " ++ getHour (nextFTHour fc)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then numeralEn clock else "midnight"
		| h == 12			= if style==1 then numeralEn 12 else "noon"
		| otherwise			= numeralEn h
	getMin :: Int -> String
	getMin m
		| m == 30			= "half"
		| m `elem` [15, 45]	= "quarter"
		| otherwise			= numeralEn m

-- FuzzyTimer

showFuzzyTimeEn (FuzzyTimer _ mins)
	| mins > 0	= "in " ++ showHelper
	| mins == 0	= "now!"
	| mins < 0	= "! " ++ showHelper ++ " ago !"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralEn hours ++ (if half then " and a half" else "") ++ " hours"
		| mm == 90	= "an hour and a half"
		| mm == 75	= "an hour and a quarter"
		| mm == 60	= "an hour"
		| mm == 45	= "three quarters"
		| mm == 30	= "half an hour"
		| mm == 15	= "a quarter"
		| mm > 1	= numeralEn mm ++ " minutes"
		| mm == 1	= "a minute"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralEn ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralEn :: Int -> String
numeralEn n
	| n < 20			= numeralEnHelper1 n
	| n `mod` 10 == 0	= numeralEnHelper10 (n `div` 10)
	| otherwise			= numeralEnHelper10 (n `div` 10) ++ "-" ++ numeralEnHelper1 (n `mod` 10)
	where
	numeralEnHelper1 :: Int -> String
	numeralEnHelper1 i = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! (i-1)
	numeralEnHelper10 :: Int -> String
	numeralEnHelper10 i = ["twenty", "thirty", "forty", "fifty"] !! (i-2)
