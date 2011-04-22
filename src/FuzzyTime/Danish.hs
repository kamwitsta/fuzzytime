-- Danish (thanks M_ller from bbs.archlinux.org) ----------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.Danish (showFuzzyTimeDa) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeDa ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeDa :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeDa fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeDaHlp fc)
showFuzzyTimeDa ft@(FuzzyTimer _ _) = showFuzzyTimeDaHlp ft

showFuzzyTimeDaHlp :: FuzzyTime -> String
showFuzzyTimeDaHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0	= getHour hour
	| min < 30	= getMin min ++ " over " ++ getHour hour
	| min == 30	= "halv " ++ getHour (nextFTHour fc)
	| min > 30	= getMin (60-min) ++ " i " ++ getHour (nextFTHour fc)
	| otherwise	= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then
								numeralDa clock
								else
								if min /=30 then "midnat" else numeralDa clock
		| otherwise			= numeralDa h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kvart"
		| otherwise			= numeralDa m

-- FuzzyTimer

showFuzzyTimeDaHlp (FuzzyTimer _ _) = "Danish is not yet available in the timer mode.\nIf you can provide a translation, please contact kamil.stachowski@gmail.com."


-- numeralDa ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralDa :: Int -> String
numeralDa n
	| n < 20			= numeralDaHelper1 n
	| n `mod` 10 == 0	= numeralDaHelper10 (n `div` 10)
	| otherwise			= numeralDaHelper1 (n `mod` 10) ++ "og" ++ numeralDaHelper10 (n `div` 10)
	where
	numeralDaHelper1 :: Int -> String
	numeralDaHelper1 i = ["en", "to", "tre", "fire", "fem", "seks", "syv", "otte", "ni", "ti", "elleve", "tolv", "tretten", "fjorten", "femten", "seksten", "sytten", "atten", "nitten"] !! (i-1)
	numeralDaHelper10 :: Int -> String
	numeralDaHelper10 i = ["tyve", "tredive", "fyrre", "halvtreds"] !! (i-2)
