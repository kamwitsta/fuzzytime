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


-- FuzzyTime ======================================================================================================================================================================


-- | Data for fuzzified time. Only keeps hour, minutes (both as Ints), clock (12 vs. 24-hour), night (Bool) and language.
-- Can be created from CalendarTime with toFuzzyTime. The String output (the \"ten past six\"-style) is obtained through Show.
data FuzzyTime = FuzzyTime {
	  fzClock	:: Int
	, fzHour	:: Int
	, fzLang	:: String
	, fzMin		:: Int
	, fzNight	:: Bool
	, fzStyle	:: Int
	} deriving (Eq)


-- | This is where FuzzyTime Int Int String is turned into the time String.
-- It is assumed that by the time these functions are called, hour will be in [0..23] and min will be in [0..59].
instance Show FuzzyTime where
	show ft@(FuzzyTime _ _ lang _ _ _) = case lang of
		"da" -> showFuzzyTimeDa ft
		"de" -> showFuzzyTimeDe ft
		"el" -> showFuzzyTimeEl ft
		"en" -> showFuzzyTimeEn ft
		"fr" -> showFuzzyTimeFr ft
		"nl" -> showFuzzyTimeNl ft
		"pl" -> showFuzzyTimePl ft
		"tr" -> showFuzzyTimeTr ft
		otherwise -> "Language " ++ lang ++ " is not supported."


-- | Converts CalendarTime to FuzzyTime using the given precision. The language and clock are also set, so that Show knows how to display it.
toFuzzyTime :: FuzzyTimeConf -> FuzzyTime
toFuzzyTime (FuzzyTimeConf cClock cLang cPrec cTime cStyle) =
	FuzzyTime cClock fuzzdHour cLang (fuzzdMin min) (hour < 10 || hour > 22) cStyle
	where
	brokenCTime :: (String, String)
	brokenCTime = break (==':') cTime
	hour :: Int
	hour = read $ fst brokenCTime
	min :: Int
	min = read $ drop 1 (snd brokenCTime)
	fuzzdHour :: Int
	fuzzdHour =
		if cClock==24 then
			if hour==0 then 24 else hour
			else
			if hour==12 then hour else hour `mod` 12
	fuzzdMin :: Int -> Int
	fuzzdMin m =
		let
			mf = fromIntegral m
			cf = fromIntegral cPrec
		in
			(round(mf/cf) * cPrec) `mod` 60


-- usability functions ============================================================================================================================================================


-- | Makes sure that midnight is always represented as 0 or 24, depending on the clock, and noon always as 12.
nextFTHour :: FuzzyTime -> Int
nextFTHour (FuzzyTime clock hour _ _ night _)
	| clock == 12 && hour == 11		= if night then 0 else 12
	| clock == 12 && hour == 12		= 1
	| clock == 24 && hour == 24		= 1
	| otherwise						= hour + 1


-- FuzzyTimeConf ==================================================================================================================================================================


-- | Four options can be set via the command line: clock (12 vs. 24), language, precision and time.
data FuzzyTimeConf = FuzzyTimeConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	} deriving (Data, Show, Typeable)
