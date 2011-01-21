{-# LANGUAGE DeriveDataTypeable #-}

module FuzzyTime (
	  FuzzyTime (..)
	, toFuzzyTime
	, nextFTHour
	, FuzzyTimeConf (..)
	) where


import Data.Data

import FuzzyTime.Danish
import FuzzyTime.Dutch
import FuzzyTime.English
import FuzzyTime.German
import FuzzyTime.Greek
import FuzzyTime.French
import FuzzyTime.Polish
import FuzzyTime.Turkish


-- FuzzyTime =======================================================================================================================================================================


-- | Data for fuzzified time. There are two modes: FuzzyClock for showing what time it is and FuzzyTimer for showing how much time there is left till something. The String output is obtained via Show.
data FuzzyTime
	= FuzzyClock {
	  ftClock	:: Int
	, ftHour	:: Int
	, ftLang	:: String
	, ftMin		:: Int
	, ftNight	:: Bool
	, ftStyle	:: Int
	}
	| FuzzyTimer {
	  ftLang	:: String
	, ftMins	:: Int
	}
	deriving Eq


-- | This is where FuzzyClock Int Int String is turned into the time String.
-- It is assumed that by the time these functions are called, hour will be in [0..23] and min will be in [0..59].
instance Show FuzzyTime where
	show ft = case ftLang ft of
		"de" -> showFuzzyTimeDe ft
		"el" -> showFuzzyTimeEl ft
		"en" -> showFuzzyTimeEn ft
		"fr" -> showFuzzyTimeFr ft
		"nl" -> showFuzzyTimeNl ft
		"pl" -> showFuzzyTimePl ft
		"tr" -> showFuzzyTimeTr ft
		otherwise -> "Language " ++ ftLang ft ++ " is not supported."


-- | Turns the config into a FuzzyTime instance. Works for both FuzzyClock and FuzzyTimer. Apart from the time, clock (12 vs. 24-hour), language, night (isNight?) and style are set so that show knows how to display the time.
toFuzzyTime :: FuzzyTimeConf -> FuzzyTime
toFuzzyTime ftc = case ftc of
	cc@(ClockConf cClock cLang cPrec cTime cStyle)
		-> FuzzyClock cClock fuzzdHour cLang fuzzdMin night cStyle
			where
			fuzzdHour :: Int
			fuzzdHour = let hh = if min+cPrec>=60 && fuzzdMin==0 then hour+1 else hour in
				if cClock==24 then
					if hh==0 then 24 else hh
				else
					if hh==12 then hh else hh `mod` 12
			fuzzdMin :: Int
			fuzzdMin =
				let
					mf = fromIntegral min
					cf = fromIntegral cPrec
				in
					(round(mf/cf) * cPrec) `mod` 60
			hour :: Int
			hour = read $ fst (break (==':') cTime)
			min :: Int
			min = read $ drop 1 $ snd (break (==':') cTime)
			night :: Bool
			night = fuzzdHour < 10 || fuzzdHour > 22
	tc@(TimerConf cEnd cLang cNow)
		-> FuzzyTimer cLang fuzzdMins
			where
			fuzzdMins :: Int
			fuzzdMins = 
				let
					mf = fromIntegral minsDiff
					cf = fromIntegral getPrec
				in
					round(mf/cf) * getPrec
			hourNow :: Int
			hourNow = read $ fst (break (==':') cNow)
			minNow :: Int
			minNow = read $ drop 1 $ snd (break (==':') cNow)
			hourEnd :: Int
			hourEnd = read $ fst (break (==':') cEnd)
			minEnd :: Int
			minEnd = read $ drop 1 $ snd (break (==':') cEnd)
			minsDiff :: Int
			minsDiff = (hourEnd*60 + minEnd) - (hourNow*60 + minNow)
			getPrec :: Int
			getPrec
				| abs minsDiff > 270	= 60
				| abs minsDiff > 90		= 30
				| abs minsDiff > 45		= 15
				| abs minsDiff > 5		= 5
				| otherwise				= 1

			

-- | Makes sure that midnight is always represented as 0 or 24 (depending on the clock) and noon always as 12.
nextFTHour :: FuzzyTime -> Int
nextFTHour (FuzzyClock clock hour _ _ night _)
	| clock == 12 && hour == 11		= if night then 0 else 12
	| clock == hour					= 1
	| otherwise						= hour + 1


-- FuzzyTimeConf ===================================================================================================================================================================


data FuzzyTimeConf
	= ClockConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	}
	| TimerConf {
	  end	:: String
	, lang	:: String
	, now	:: String
	}
	deriving (Data, Show, Typeable)
