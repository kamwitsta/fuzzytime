-- Swedish (thanks Closey from forums.gentoo.org) ---------------------------------------------------------------------------------------------------------------------------------


module LibFuzzyTime.Swedish (showFuzzyTimeSe) where

import {-# SOURCE #-} LibFuzzyTime
import Prelude hiding (min)


-- showFuzzyTimeSe ----------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeSe :: FuzzyTime -> String

-- FuzzyClock

showFuzzyTimeSe fc@(FuzzyClock _ caps _ _ _ _ _) = capsizeDef caps (showFuzzyTimeSeHlp fc)
showFuzzyTimeSe ft@(FuzzyTimer _ _) = showFuzzyTimeSeHlp ft

showFuzzyTimeSeHlp :: FuzzyTime -> String
showFuzzyTimeSeHlp fc@(FuzzyClock _ _ clock hour _ min style)
	| min == 0				= getHour hour
	| min `elem` [23..29]	= getMin (30-min) ++ " i halv " ++ getHour (nextFTHour fc)
	| min < 30				= getMin min ++ " över " ++ getHour hour
	| min `elem` [31..37]	= getMin (min-30) ++ " över halv " ++ getHour (nextFTHour fc)
	| min == 30				= "halv " ++ getHour (nextFTHour fc)
	| min > 30				= getMin (60-min) ++ " i " ++ getHour (nextFTHour fc)
	| otherwise				= "Oops, looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `elem` [0, 24]	= if style==1 then numeralSe clock else "midnatt"
		| otherwise			= numeralSe h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kvart"
		| otherwise			= numeralSe m

-- FuzzyTimer

showFuzzyTimeSeHlp (FuzzyTimer _ mins)
	| mins > 0	= "om " ++ showHelper
	| mins == 0	= "nu!"
	| mins < 0	= "! för " ++ showHelper ++ " sedan !"
	where
	showHelper :: String
	showHelper
		| mm > 90	= numeralSe hours ++ (if half then " och en halv" else "") ++ " timmar"
		| mm == 90	= "en och en halv timme"
		| mm == 75	= "en timme och en kvart"
		| mm == 60	= "en timme"
		| mm == 45	= "fyrtiofem minuter"
		| mm == 30	= "halv timme"
		| mm == 15	= "en kvart"
		| mm > 1	= numeralSe mm ++ " minuter"
		| mm == 1	= "en minut"
		| otherwise	= "Oops, it looks like there's " ++ show mins ++ " left."
	hours :: Int
	hours = round $ (fromIntegral mm :: Float) / 60
	mm :: Int
	mm = abs mins
	half :: Bool
	half = mm `mod` 60 == 30


-- numeralSe ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


numeralSe :: Int -> String
numeralSe n
	| n < 20			= numeralSeHelper1 n
	| n `mod` 10 == 0	= numeralSeHelper10 (n `div` 10)
	| otherwise			= numeralSeHelper10 (n `div` 10) ++ numeralSeHelper1 (n `mod` 10)
	where
	numeralSeHelper1 :: Int -> String
	numeralSeHelper1 i = ["ett", "två", "tre", "fyra", "fem", "sex", "sju", "åtta", "nio", "tio", "elva", "tolv", "tretton", "fjorton", "femton", "sexton", "sjutton", "arton", "nitton"] !! (i-1)
	numeralSeHelper10 :: Int -> String
	numeralSeHelper10 i = ["tjugo", "trettio", "fyrtio", "femtio"] !! (i-2)
