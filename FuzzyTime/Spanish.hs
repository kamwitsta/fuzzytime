-- Spanish ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


module FuzzyTime.Spanish (showFuzzyTimeEs) where

import {-# SOURCE #-} FuzzyTime


-- showFuzzyTimeEs ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeEs :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeEs fc@(FuzzyClock clock hour _ min night style)
	| min == 0	= if getHour hour == "la medianoche" then getHour hour else getHour hour
	| min <= 30	= getHour hour ++ " y " ++ getMin min
	| min > 30	= getHour (nextFTHour fc) ++ " menos " ++ getMin (60-min)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` clock == 1= "la una"
		| h `mod` 12 == 0	= if style==1 then
								"las " ++ if clock==12 then numeralEs 12 else numeralEs h
								else
								if night then "la medianoche" else "las " ++ numeralEs 12
		| otherwise			= "las " ++ numeralEs h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "cuarto"
		| m == 30			= "media"
		| otherwise			= numeralEs m

-- FuzzyTimer

showFuzzyTimeEs (FuzzyTimer _ mins) = "Spanish is not yet available in the timer mode.\nIf you can provide a translation, please contact kamil.stachowski@gmail.com."


-- numeralDe ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


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

