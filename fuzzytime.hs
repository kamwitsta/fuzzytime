-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- |
---- Module			: Fuzzytime
---- Copyright		: (C) Kamil Stachowski <kamil.stachowski@gmail.com>
---- License		: GPL3+
---- Maintainer		: Kamil Stachowski <kamil.stachowski@gmail.com>
---- Stability		: unstable
---- Portability	: unportable
---- A clock that ells the time in a more familiar way (the \"ten past six\"-style).
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

module Main (
	-- * Description
	-- $description
	  main
	) where


import Data.Char (isDigit)
import Data.List (intercalate)
import System.Console.CmdArgs
import System.Time

import FuzzyTime


-- $description
-- A small utility to tell the current time in a more familiar way (the \"ten past six\"-style).
--
-- The whole thing works like this: the current time is read and turned into CalendarTime. This is fuzzified (i.e. the precision is reduced) and at the same time changed into FuzzyTime. FuzzyTime holds the time as two Ints (hour and min), the target language, the desired clock (12 vs. 24) and an information on whether it is am or pm (needed for the 12-hour clock). Custom functions show FuzzyTime as the \"ten past six\"-style String.
--
-- Depends on N. Mitchell's System.Console.CmdArds 0.6.4-1+.
--
-- To add a new language, two things need to be done:
--
-- (1) The new language needs to be added to confAvailLangs (in fuzzytime.hs) and to instance Show FuzzyTime (in FuzzyTime.hs).
--
-- (2) A module FuzzyTime.NewLanguge needs to be created and added to the list of imported modules in FuzzyTime.hs.


-- CHANGELOG
-- BUGS
-- 		around even hours shows one hour back
-- TODO
-- 		exit codes
-- 		answers for Greek (:35, midnight, εντεκάμιση)
--		check Danish: midnight + noon, halves
-- 0.5	2011.01.17
-- 		added halves as base (de, nl and pl)
-- 		added Greek (thanks Gbak), Dutch (thanks litemotiv) and Turkish
-- 		some corrections (thanks Daniel Fischer from beginners@haskell.org again)
-- 		0.4.1	2011.01.15
-- 				fixed nextFTHour
-- 0.4	2011.01.15
-- 		added --time (thanks Daniel Fischer and Brent Yorgey from beginners@haskell.org!)
-- 		added --style
-- 		added Danish (by M_ller with my modifications)
-- 		removed "południe" from pl
-- 		sorted out the representation of midnight and noon
-- 		added a man page
-- 0.3	2011.01.14
-- 		added midnight and noon
-- 		added checking cli options
-- 		fixed the "quarter past quarter" bug
-- 0.2	2011.01.12
-- 		added French and German
-- 		added 12 vs. 24-hour clock
-- 		0.1.1	2010.12.06
-- 				added cabal
-- 0.1	2010.12.05
-- 		initial release: two languages (en and pl), 1 < precision < 60


-- config =========================================================================================================================================================================

-- available ----------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] Available languages.
confAvailLangs :: [String]
confAvailLangs = ["da", "de", "el", "en", "fr", "nl", "pl", "tr"]

-- | Print nicely what languages are available.
showAvailLangs :: String -> String
showAvailLangs i = case length confAvailLangs of
					0			-> "none"
					1			-> head confAvailLangs
					otherwise	-> intercalate ", " (init confAvailLangs) ++ " " ++ i ++ " " ++ last confAvailLangs


-- defaults -----------------------------------------------------------------------------------------------------------------------------------------------------------------------


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
	nowClock <- getClockTime
	let nowString = take 5 . drop 11 $ show nowClock
	return $ ft { time = nowString &= help confHelpTime }


-- | \[config] Get the default config. Note that time is set to "" and can only be filled later with confSetCurrTime. This is due to getClockTime being in IO.
getDefConf :: FuzzyTimeConf
getDefConf = FuzzyTimeConf {
	  clock	= confDefClock	&= help confHelpClock
	, lang	= confDefLang	&= help confHelpLang
	, prec	= confDefPrec	&= help confHelpPrec
	, time	= ""
	, style	= confDefStyle	&= help confHelpStyle
	}
	&= program confHelpProgram
	&= summary confHelpSummary


-- help ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] Help message for clock
confHelpClock :: String
confHelpClock = "12 or 24-hour clock; default " ++ show confDefClock ++ "-hour."

-- | \[config] Help message for lang
confHelpLang :: String
confHelpLang = "Language (currently " ++ showAvailLangs "and" ++ "); default " ++ confDefLang ++ "."

-- | \[config] Help message for prec
confHelpPrec :: String
confHelpPrec = "Precision (1 <= prec <= 60 [minutes]); default " ++ show confDefPrec ++ "."

-- | \[config] Help message for time
confHelpTime :: String
confHelpTime = "Time to fuzzify as HH:MM; default current time." 

-- | \[config] Help message for style
confHelpStyle :: String
confHelpStyle = "How the time is told (seem the man page); default " ++ show confDefStyle ++ "."

-- | \[config] Help message for program
confHelpProgram :: String
confHelpProgram = "fuzzytime"

-- | \[config] Help message for summary
confHelpSummary :: String
confHelpSummary = "A clock that tells the time in a more familiar way.\nv0.5, 2011.01.17, kamil.stachowski@gmail.com, GPL3+"


-- check --------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] Check that arguments given at cli are correct.
checkFTConf :: FuzzyTimeConf -> Either String FuzzyTimeConf
checkFTConf ftc@(FuzzyTimeConf clock lang prec time style)
	| clock `notElem` [12, 24]		= Left "--clock must be either 12 or 24."
	| lang `notElem` confAvailLangs	= Left ("--lang must be " ++ showAvailLangs "or" ++ ".")
	| prec < 1 || prec > 60			= Left "--prec must be 1 < prec < 60."
	| not checkTimeOk				= Left "--time must be given as HH:MM, where HH is in [0..23] and MM is in [0..59]"
	| style `notElem` [1, 2]		= Left "--style must be either 1 or 2 (see the man page)."
	| otherwise						= Right ftc
	where
	checkTimeOk :: Bool
	checkTimeOk = case break (== ':') time of
		(hh, _:mm)	->	not (null hh || null mm)
						&& all isDigit hh && all isDigit mm
						&& (let h = read hh; m = read mm
							in 0 <= h && h < 24 && 0 <= m && m < 60)
		_			->	False


-- main ===========================================================================================================================================================================


-- | The main part. Only reads the command line args and the current time, fuzzifies and prints it.
main :: IO ()
main = do
	c <- cmdArgs =<< confSetCurrTime getDefConf		-- ^ confSetCurrTime required to avoid unsafePerformIO
	case checkFTConf c of
		Left msg	-> putStrLn msg
		Right conf	-> print $ toFuzzyTime conf
