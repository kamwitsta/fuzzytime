-- Spanish (thanks xenofungus and itsbrad212 from bbs.archlinux.org) --------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-tabs #-}

module FuzzyTime.Spanish (showFuzzyTimeEs) where

import {-# SOURCE #-} FuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeEs ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeEs :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeEs fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeEsHlp fc)
showFuzzyTimeEs ft@(FuzzyTimer _ _) = showFuzzyTimeEsHlp ft

showFuzzyTimeEsHlp :: FuzzyTime -> String
showFuzzyTimeEsHlp fc@(FuzzyClock am _ clock hour _ min style)
	| min == 0	= if getHour hour == "la medianoche" then getHour hour else getHour hour ++ getAm hour
	| min <= 30	= getHour hour ++ " y " ++ getMin min ++ getAm hour
	| min > 30	= getHour (nextFTHour fc) ++ " menos " ++ getMin (60-min) ++ getAm (nextFTHour fc)
	| otherwise	= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getAm :: Int -> String
	getAm h =
		if style==3 && getHour h /= "la medianoche" then
			if hh < 13 then " de la mañana" else
				if hh < 21 then " de la tarde" else " de la noche"
			else ""
		where
		hh :: Int
		hh = if clock==12 && h < 12 && not am then h+12 else h
	getHour :: Int -> String
	getHour h
		| h `mod` clock == 1= "la una"
		| h `elem` [0, 24]	= if style==1 then "las " ++ numeralEs clock else "la medianoche"
		| otherwise			= "las " ++ numeralEs h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "cuarto"
		| m == 30			= "media"
		| otherwise			= numeralEs m

-- FuzzyTimer

showFuzzyTimeEsHlp (FuzzyTimer _ _) = "Spanish is not yet available in the timer mode.\nIf you can provide a translation, please contact kamil.stachowski@gmail.com."


-- numeralEs ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralEs :: Int -> String
numeralEs n
	| n < 20			= numeralEsHelper1 n
	| n == 20			= "veinte"
	| n == 21			= "veintiuno"
	| n == 22			= "veintidós"
	| n == 23			= "veintitrés"
	| n == 26			= "veintiséis"
	| n < 30 			= "veinti" ++ numeralEsHelper1 (n `mod` 10)
	| n `mod` 10 == 0	= numeralEsHelper10 (n `div` 10)
	| otherwise			= numeralEsHelper10 (n `div` 10) ++ " y " ++ numeralEsHelper1 (n `mod` 10)
	where
	numeralEsHelper1 :: Int -> String
	numeralEsHelper1 i = ["una", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"] !! (i-1)
	numeralEsHelper10 :: Int -> String
	numeralEsHelper10 i = ["treinta", "cuarenta", "cincuenta"] !! (i-3)

