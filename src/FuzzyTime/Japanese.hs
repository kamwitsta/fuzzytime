-- Japanese (petersen at fedoraproject.org) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.Japanese (showFuzzyTimeJa) where

import {-# SOURCE #-} FuzzyTime
import Data.Char (toLower, toUpper)
import Data.List (intersperse)
import Prelude hiding (min)


-- showFuzzyTimeJa ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeJa :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeJa fc@(FuzzyClock am _ clock hour _ min style)
	| min == 0	= getHour hour ++ "時"
	| min < 45	= getHour hour ++ "時" ++ getMin min ++ ""
	| min >= 45	= getHour (nextFTHour fc) ++ "時" ++ getMin (60-min) ++ "前"
	| otherwise	= "あら、" ++ show hour ++ "時" ++ show min ++ "分です"
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then numeralJa clock else "零時"
		| h == 12		= if style==1 then numeralJa 12 else "正午"
		| otherwise		= numeralJa h
	getMin :: Int -> String
	getMin m
		| m == 30		= "半"
		| otherwise		= numeralJa m ++ "分"

-- FuzzyTimer

showFuzzyTimeJa (FuzzyTimer _ mins)
	| mins > 0	= "後 " ++ showHelper
	| mins == 0	= "今です！"
	| mins < 0	= "! " ++ showHelper ++ "前！"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralJa hours ++ "時間" ++ (if half then "半" else "")
		| mm == 90	= "一時間半"
		| mm == 75	= "一時間十五分"
		| mm == 60	= "一時間"
		| mm == 45	= "四十五分"
		| mm == 30	= "三十分"
		| mm == 15	= "十五分"
		| mm >= 1	= numeralJa mm ++ "分"
		| otherwise	= "あら、残り " ++ show mins ++ "分"
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralJa ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralJa :: Int -> String
numeralJa n
	| n < 20			= numeralJaHelper1 n
	| n `mod` 10 == 0	= numeralJaHelper10 (n `div` 10)
	| otherwise			= numeralJaHelper10 (n `div` 10) ++ numeralJaHelper1 (n `mod` 10)
	where
	numeralJaHelper1 :: Int -> String
	numeralJaHelper1 i = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十一", "十二", "十三", "十四", "十五", "十六", "十七", "十八", "十九"] !! (i-1)
	numeralJaHelper10 :: Int -> String
	numeralJaHelper10 i = ["二十", "三十", "四十", "五十"] !! (i-2)
