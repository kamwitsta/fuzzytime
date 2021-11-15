-- Turkish ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-tabs #-}

module FuzzyTime.Turkish (showFuzzyTimeTr) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeTr ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeTr :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeTr fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeTrHlp fc)
showFuzzyTimeTr ft@(FuzzyTimer _ _) = showFuzzyTimeTrHlp ft

showFuzzyTimeTrHlp :: FuzzyTime -> String
showFuzzyTimeTrHlp ft@(FuzzyClock _ _ clock hour _ min style)
	| min == 0			= "saat " ++ getHour Nom hour
	| min `elem` [20..29]
		&& style == 2	= getHour Nom hour ++ " buçuğa " ++ getMin (30-min) ++ " var"
	| min < 30			= getHour Acc hour ++ " " ++ getMin min ++ " geçiyor"
	| min == 30			= if hour `mod` 12 == 0 then "saat yarım" else getHour Nom hour ++ " buçuk"
	| min `elem` [31..40]
		&& style == 2	= getHour Nom hour ++ " buçuğu " ++ getMin (min-30) ++ " geçiyor"
	| min > 30			= getHour Dat (nextFTHour ft) ++ " " ++ getMin (60-min) ++ " var"
	| otherwise			= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Case -> Int -> String
	getHour c h
		| h `mod` 12 == 0	= if clock==12 then numeralTr c 12 else numeralTr c h
		| otherwise			= numeralTr c h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "çeyrek"
		| otherwise			= numeralTr Nom m

-- FuzzyTimer

showFuzzyTimeTrHlp (FuzzyTimer _ mins)
	| mins > 0	= showHelper ++ " sonra"
	| mins == 0	= "şimdi!"
	| mins < 0	= "! " ++ showHelper ++ " önce !"
	where
	showHelper :: String
	showHelper
		| mm > 75	= numeralTr Nom hours ++ (if half then " buçuk" else "") ++ " saat"
		| mm == 75	= "bir saat çeyrek"
		| mm == 60	= "bir saat"
		| mm == 45	= "üç çeyrek"
		| mm == 30	= "yarım saat"
		| mm == 15	= "bir çeyrek"
		| mm >= 1	= numeralTr Nom mm ++ " dakika"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralTr ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


data Case = Nom | Dat | Acc


numeralTr :: Case -> Int -> String
numeralTr c n
	| n < 10			= numeralTrHelper1 c n
	| n `mod` 10 == 0	= numeralTrHelper10 c (n `div` 10)
	| otherwise			= numeralTrHelper10 Nom (n `div` 10) ++ " " ++ numeralTrHelper1 c (n `mod` 10)
	where
	numeralTrHelper1 :: Case -> Int -> String
	numeralTrHelper1 Nom i = ["bir", "iki", "üç", "dört", "beş", "altı", "yedi", "sekiz", "dokuz"] !! (i-1)
	numeralTrHelper1 Dat i = ["bire", "ikiye", "üçe", "dörde", "beşe", "altıya", "yediye", "sekize", "dokuza"] !! (i-1)
	numeralTrHelper1 Acc i = ["biri", "ikiyi", "üçü", "dördü", "beşi", "altıyı", "yediyi", "sekizi", "dokuzu"] !! (i-1)
	numeralTrHelper10 :: Case -> Int -> String
	numeralTrHelper10 Nom i = ["on", "yirmi", "otuz", "kırk", "elli"] !! (i-1)
	numeralTrHelper10 Dat i = ["ona", "yirmiye", "otuza", "kırka", "elliye"] !! (i-1)
	numeralTrHelper10 Acc i = ["onu", "yirmiyi", "otuzu", "kırkı", "elliyi"] !! (i-1)
