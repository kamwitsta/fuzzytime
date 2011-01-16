{-# LANGUAGE DeriveDataTypeable #-}

module FuzzyTime (
	  FuzzyTime (..)
	, FuzzyTimeConf (..)
	, checkFTConf
	, getFTConf
	, nextFTHour
	, toFuzzyTime
	) where

import Data.Char (isDigit)
import System.Console.CmdArgs
import System.Time

import FuzzyTime.Danish
import FuzzyTime.Dutch
import FuzzyTime.English
import FuzzyTime.German
import FuzzyTime.Greek
import FuzzyTime.French
import FuzzyTime.Polish
import FuzzyTime.Turkish

-- config =========================================================================================================================================================================


-- | \[config] The default clock (12 vs. 24-hour).
confDefClock :: Int
confDefClock = 12

-- | \[config] The default language.
confDefLang :: String
confDefLang = "en"

-- | \[config] The default precision (should be in [1..60]).
confDefPrec :: Int
confDefPrec = 5

-- | \[config] The default style (see man).
confDefStyle :: Int
confDefStyle = 1

-- | \[config] The default time (current).
confSetCurrTime :: FuzzyTimeConf -> IO FuzzyTimeConf
confSetCurrTime ft = do
	now <- getClockTime
	return $ ft { time = take 5 . drop 11 $ show now }
-- cl args ========================================================================================================================================================================


-- | Four options can be set via the command line: clock (12 vs. 24), language, precision and time.
data FuzzyTimeConf = FuzzyTimeConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	} deriving (Show, Data, Typeable)


-- | Check that arguments given at cli are correct.
checkFTConf :: FuzzyTimeConf -> String
checkFTConf (FuzzyTimeConf clock lang prec time style)
	| clock `notElem` [12, 24]	= "--clock must be either 12 or 24."
	| lang `notElem` ["da", "de", "el", "en", "fr", "nl", "pl", "tr"]	= "--lang must be de, el, en, fr, nl, pl or tr."
	| prec < 1 || prec > 60			= "--prec must be 1 < prec < 60."
	| not checkTimeOk				= "--time must be given as HH:MM, where HH is in [0..23] and MM is in [0..59]"
	| style `notElem` [1, 2]		= "--style must be either 1 or 2 (see the man page)."
	| otherwise						= "ok"
	where
	checkTimeOk :: Bool
	checkTimeOk = case break (== ':') time of
		(hh, _:mm)	->	not (null hh || null mm)
						&& all isDigit hh && all isDigit mm
						&& (let h = read hh; m = read mm
							in 0 <= h && h < 24 && 0 <= m && m < 60)
		_			->	False


-- | Fill the config with the default values.
getFTConf :: FuzzyTimeConf
getFTConf = FuzzyTimeConf {
	  clock	= confDefClock	&= help "12 or 24-hour clock; default 12-hour."
	, lang	= confDefLang	&= help "Language (currently da, de, en, fr, nl, pl and tr); default en."
	, prec	= confDefPrec	&= help "Precision (1 <= prec <= 60 [minutes]); default 5."
	, time	= ""			&= help "Time to fuzzify as HH:MM; default current time."			-- ^ time is set via confSetCurrTime to avoid unsafePerformIO
	, style	= confDefStyle	&= help "How the time is told (seem the man page); default 1."
	}
	&= program "fuzzytime"
	&= summary "A clock that tells the time in a more familiar way, e.g. 10:52 -> ten to eleven.\nv0.5, 2011.01.15, kamil.stachowski@gmail.com, GPL3+"


-- FuzzyTime – main ===============================================================================================================================================================


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


-- | Makes sure that midnight is always represented as 0 or 24, depending on the clock, and noon always as 12.
nextFTHour :: FuzzyTime -> Int
nextFTHour (FuzzyTime clock hour _ _ night _)
	| clock == 12 && hour == 11		= if night then 0 else 12
	| clock == 12 && hour == 12		= 1
	| clock == 24 && hour == 24		= 1
	| otherwise						= hour + 1
