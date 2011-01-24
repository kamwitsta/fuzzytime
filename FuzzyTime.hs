{- |
In this module the actual conversion from time to string happens.

There are two modes, showing the time and showing the time left till something. This is represented by the duality of both FuzzyTimeConf and FuzzyTime: ClockConf and FuzzyClock serve to show the time, while TimerConf and FuzzyTimer are used for counting down.

A FuzzyTime is created from a FuzzyTimeConf with toFuzzyTime. It is translated to a string via show.

Apart from the above, two functions are exported: isTimerZero which can be used by an interface to set off the alarm when timer reaches zero, and nextFTHour which makes sure that the clock is a circular data structure.
-}


{-# LANGUAGE DeriveDataTypeable #-}

module FuzzyTime (
	  FuzzyTime (..)
	, toFuzzyTime
	, isTimerZero
	, nextFTHour
	, FuzzyTimeConf (..)
	, Time
	) where


import Data.Data

import FuzzyTime.Danish
import FuzzyTime.Dutch
import FuzzyTime.English
import FuzzyTime.German
import FuzzyTime.Greek
import FuzzyTime.French
import FuzzyTime.Norwegian
import FuzzyTime.Polish
import FuzzyTime.Spanish
import FuzzyTime.Turkish


-- FuzzyTime =======================================================================================================================================================================


-- | Convenience alias.
type Time = String



-- | Data for fuzzified time. There are two modes: FuzzyClock for showing what time it is and FuzzyTimer for showing how much time there is left till something. The String output is obtained via Show.
data FuzzyTime
	= FuzzyClock {
	  ftAm		:: Bool
	, ftClock	:: Int
	, ftHour	:: Int
	, ftLang	:: String
	, ftMin		:: Int
	, ftStyle	:: Int
	}
	| FuzzyTimer {
	  ftLang	:: String
	, ftMins	:: Int
	}
	deriving Eq


-- | This is where FuzzyTime is turned into the time String.
-- It is assumed that by the time these functions are called, hour will be in [0..23] and min will be in [0..59].
instance Show FuzzyTime where
	show ft = case ftLang ft of
		"da" -> showFuzzyTimeDa ft
		"de" -> showFuzzyTimeDe ft
		"el" -> showFuzzyTimeEl ft
		"en" -> showFuzzyTimeEn ft
		"es" -> showFuzzyTimeEs ft
		"fr" -> showFuzzyTimeFr ft
		"nb" -> showFuzzyTimeNb ft
		"nl" -> showFuzzyTimeNl ft
		"pl" -> showFuzzyTimePl ft
		"tr" -> showFuzzyTimeTr ft
		otherwise -> "Language " ++ ftLang ft ++ " is not supported."


-- | Turns a FuzzyTimeConf into a FuzzyTime. Works for both FuzzyClock and FuzzyTimer.
-- In the clock mode, am (Bool), clock (12 vs. 24-hour), language and style are set apart from the actual time, so that show knows how to display the time.
-- In the timer mode, only language and left minutes need to be set.
toFuzzyTime :: FuzzyTimeConf -> FuzzyTime
toFuzzyTime ftc = case ftc of
	cc@(ClockConf cClock cLang cPrec cTime cStyle)
		-> FuzzyClock am cClock fuzzdHour cLang fuzzdMin cStyle
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
			am :: Bool
			am = hour < 12
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


-- | Makes sure that noon is always represented as 0, and midnight – always as 0 or 24 (depending on the clock).
nextFTHour :: FuzzyTime -> Int
nextFTHour (FuzzyClock am clock hour _ _  _)
	| clock == 12 && hour == 11		= if am then 12 else 0
	| clock == hour					= 1
	| otherwise						= hour + 1


-- | Reports whether timer is now at zero. (Needed for the interface to know when to play a sound.)
isTimerZero :: FuzzyTime -> Bool
isTimerZero (FuzzyClock _ _ _ _ _ _)	= False
isTimerZero (FuzzyTimer _ mins)			= mins == 0


-- FuzzyTimeConf ===================================================================================================================================================================


-- | Data for CmdArgs. Has the two modes of module FuzzyTime: showing the current time and showing the time left. Note that this is not the same as the two modes of module Main.
data FuzzyTimeConf
	= ClockConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: Time
	, style	:: Int
	}
	| TimerConf {
	  end	:: Time
	, lang	:: String
	, now	:: Time
	}
	deriving (Data, Show, Typeable)
