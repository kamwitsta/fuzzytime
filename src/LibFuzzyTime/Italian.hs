-- Italian (thanks erm67 from forums.gentoo.org) ----------------------------------------------------------------------------------------------------------------------------------


module LibFuzzyTime.Italian (showFuzzyTimeIt) where

import {-# SOURCE #-} LibFuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeIt ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeIt :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeIt fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeItHlp fc)
showFuzzyTimeIt ft@(FuzzyTimer _ _) = showFuzzyTimeItHlp ft

showFuzzyTimeItHlp :: FuzzyTime -> String
showFuzzyTimeItHlp fc@(FuzzyClock am _ clock hour _ min style)
	| min == 0	= if getHour hour `elem` ["mezzanotte", "mezzogiorno"] then getHour hour else getHour hour ++ getAm hour
	| min <= 30	= getHour hour ++ " e " ++ getMin min ++ getAm hour
	| min > 30	= getHour (nextFTHour fc) ++ " meno " ++ getMin (60-min) ++ getAm (nextFTHour fc)
	| otherwise	= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getAm :: Int -> String
	getAm h =
		if style==2 && (getHour h) `notElem` ["mezzanotte", "mezzogiorno"] then
			if hh < 5 then " di notte" else
				if hh < 13 then " del mattino" else
					if hh < 19 then " del pomeriggio" else " di sera"
			else ""
		where
		hh :: Int
		hh = if clock==12 && h < 12 && not am then h+12 else h
	getHour :: Int -> String
	getHour h
		| h `mod` clock == 1= "l’una"
		| h `elem` [0, 24]	= if style==1 then "le " ++ numeralIt clock else "mezzanotte"
		| h == 12			= if style==1 then "le " ++ numeralIt clock else "mezzogiorno"
		| otherwise			= "le " ++ numeralIt h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "un quarto"
		| m == 30			= "mezzo"
		| otherwise			= numeralIt m

-- FuzzyTimer

showFuzzyTimeItHlp (FuzzyTimer _ _) = "Italian is not yet available in the timer mode.\nIf you can provide a translation, please contact kamil.stachowski@gmail.com."


-- numeralEs ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralIt :: Int -> String
numeralIt n
	| n < 20			=	numeralItHelper1 n
	| n `mod` 10 == 0	=	numeralItHelper10 (n `div` 10)
	| otherwise			=	if (n `mod` 10) `elem` [1, 3, 8]
							then	if (n `mod` 10) `elem` [1, 8]
									then init (numeralItHelper10 (n `div` 10)) ++ numeralItHelper1 (n `mod` 10)
									else numeralItHelper10 (n `div` 10) ++ "tré"
							else numeralItHelper10 (n `div` 10) ++ numeralItHelper1 (n `mod` 10)
	where
	numeralItHelper1 :: Int -> String
	numeralItHelper1 i = ["uno", "due", "tre", "quattro", "cinque", "sei", "sette", "otto", "nove", "dieci", "undici", "dodici", "tredici", "quattordici", "quindici", "sedici", "diciasette", "diciotto", "diciannove"] !! (i-1)
	numeralItHelper10 :: Int -> String
	numeralItHelper10 i = ["venti", "trenta", "quaranta", "cinquanta"] !! (i-2)
