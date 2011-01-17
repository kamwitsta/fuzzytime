-- Greek (thanks Gbak from bbs.archlinux.org) -------------------------------------------------------------------------------------------------------------------------------------

module FuzzyTime.Greek (showFuzzyTimeEl) where

import {-# SOURCE #-} FuzzyTime


showFuzzyTimeEl :: FuzzyTime -> String
showFuzzyTimeEl ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " και " ++ getMin min
	| min > 30	= getHour (nextFTHour ft) ++ " παρά " ++ getMin (60-min)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if clock==12 then numeralEl 12 else numeralEl h
		| otherwise			= numeralEl h
	getMin :: Int -> String
	getMin m
		| m == 30			= "μισή"
		| m `elem` [15, 45]	= "τέταρτο"
		| otherwise			= numeralEl m


numeralEl :: Int -> String
numeralEl n
	| n < 20			= numeralElHelper1 n
	| n `mod` 10 == 0	= numeralElHelper10 (n `div` 10)
	| otherwise			= numeralElHelper10 (n `div` 10) ++ " " ++ numeralElHelper1 (n `mod` 10)
	where
	numeralElHelper1 :: Int -> String
	numeralElHelper1 i = ["ένας", "δυο", "τρεις", "τέσσερις", "πέντε", "έξι", "εφτά", "οχτώ", "εννιά", "δέκα", "έντεκα", "δώδεκα", "δεκατρείς", "δεκατέσσερις", "δεκαπέντε", "δεκάξι", "δεκαεφτά", "δεκαοχτώ", "δεκαεννιά"] !! (i-1)
	numeralElHelper10 :: Int -> String
	numeralElHelper10 i = ["είκοσι", "τριάντα", "σαράντα", "πενήντα"] !! (i-2)
