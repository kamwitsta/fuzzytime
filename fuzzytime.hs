-- fuzzytime timer (no END) when timer is set

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

{-# LANGUAGE DeriveDataTypeable #-}

module Main (
	-- * Description
	-- $description
	  main
	) where


import Data.Char (isDigit)
import Data.List (intercalate)
import Directory (removeFile)
import System.Console.CmdArgs
import System.Environment (getEnv)
import System.Time (getClockTime)

import FuzzyTime


-- $description
-- fuzzytime is a small utility which tells what time it is or how much time there is left to something, in a more familiar way such as "ten to six" rather than 17:51 (telling the time), or "in five minutes" (telling the left time).
--
-- There are two modes: telling the time / the left time, and setting the timer. See main below.
--
-- Depends on N. Mitchell's System.Console.CmdArds 0.6.4-1+.
--
-- To add a new language, two things need to be done:
--
-- (1) The new language needs to be added to confAvailLangs (in fuzzytime.hs) and to instance Show FuzzyTime (in FuzzyTime.hs).
--
-- (2) A module FuzzyTime.NewLanguge needs to be created and added to the list of imports in FuzzyTime.hs.


-- CHANGELOG
-- TODO
-- 		exit codes
-- 		answers for Greek (:35, midnight, εντεκάμιση)
--		check Danish: midnight + noon, halves
--		Spanish
-- 0.6	2011.01.21
-- 		added the timer mode
-- 		fixed the almost-next-hour bug
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


-- | \[config] Languages available.
confAvailLangs :: [String]
confAvailLangs = ["da", "de", "el", "en", "fr", "nl", "pl", "tr"]


-- defaults -----------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] The default language.
confDefLang :: String
confDefLang = "en"

-- | \[config] The default clock (12 vs. 24-hour) for the clock mode.
confDefClockClock :: Int
confDefClockClock = 12

-- | \[config] The default precision (should be in [1..60]) for the clock mode.
confDefClockPrec :: Int
confDefClockPrec = 5

-- | \[config] The default style for the clock mode (see the man page).
confDefClockStyle :: Int
confDefClockStyle = 1


-- | \[config] Get the default config for showing the time.
getDefClockConf :: IO FuzzyTimeConf
getDefClockConf = do
	nowClock <- getClockTime
	let nowString = take 5. drop 11 $ show nowClock
	return $ ClockConf {
		  clock	= confDefClockClock	&= help confHelpClockClock
		, lang	= confDefLang		&= help confHelpClockLang
		, prec	= confDefClockPrec	&= help confHelpClockPrec
		, time	= nowString			&= help confHelpClockTime
		, style	= confDefClockStyle	&= help confHelpClockStyle
		} &= name "clock" &= help confHelpClock

-- | \[config] Get the default config for either showing the time or setting the timer.
getDefTimerConf :: IO FuzzyTimeConf
getDefTimerConf = do
	nowClock <- getClockTime
	let nowString = take 5 . drop 11 $ show nowClock
	endFile <- (readFile =<< confTimerFileLoc) `catch` (\_ -> return "empty")
	let endString = take 5 endFile
	return $ TimerConf {
		  end	= endString			&= args &= typ "END"
		, lang	= confDefLang		&= ignore
		, now	= nowString			&= ignore
		} &= name "timer" &= help confHelpTimer


-- internal -----------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] The default location for the timer file.
confTimerFileLoc :: IO String
confTimerFileLoc = do
	home <- getEnv "HOME"
	return $ home ++ "/.fuzzytimer"


-- | Print nicely what languages are available.
showAvailLangs :: String -> String
showAvailLangs i = case length confAvailLangs of
						0	-> "none"
						1	-> head confAvailLangs
						_	-> intercalate ", " (init confAvailLangs) ++ " " ++ i ++ " " ++ last confAvailLangs


-- help ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] Help message for the clock (showing) mode.
confHelpClock :: String
confHelpClock = "Print fuzzy time. (Disabled if timer is set.)"

-- | \[config] Help message for --clock.
confHelpClockClock :: String
confHelpClockClock = "12 or 24-hour clock; default " ++ show confDefClockClock ++ "-hour."

-- | \[config] Help message for --lang.
confHelpClockLang :: String
confHelpClockLang = "Language (currently " ++ showAvailLangs "and" ++ "); default " ++ confDefLang ++ "."

-- | \[config] Help message for --prec.
confHelpClockPrec :: String
confHelpClockPrec = "Precision (1 <= prec <= 60 [minutes]); default " ++ show confDefClockPrec ++ "."

-- | \[config] Help message for --time.
confHelpClockTime :: String
confHelpClockTime = "Time to fuzzify as HH:MM; default current time." 

-- | \[config] Help message for --style.
confHelpClockStyle :: String
confHelpClockStyle = "How the time is told (seem the man page); default " ++ show confDefClockStyle ++ "."


-- | \[config] Help message for the timer (setting) mode.
confHelpTimer :: String
confHelpTimer = "Set timer to END as HH:MM or \"unset\". (Disables clock.)"

-- | \[config] Help message for END.
confHelpTimerEnd :: String
confHelpTimerEnd = "Set the timer to countdown to END as HH:MM; obligatory."


-- | \[config] Help message for program
confHelpProgram :: String
confHelpProgram = "fuzzytime"

-- | \[config] Help message for summary
confHelpSummary :: String
confHelpSummary = "A clock and timer that tell the time in a more familiar way.\nv0.6, 2011.01.21, kamil.stachowski@gmail.com, GPL3+"


-- check --------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | \[config] Check that arguments given at cli or read from the timer file are correct.
checkFTConf :: FuzzyTimeConf -> Either String FuzzyTimeConf

checkFTConf c@(ClockConf clock lang prec time style)
	| clock `notElem` [12, 24]	= Left "--clock must be either 12 or 24."
	| lang `notElem` confAvailLangs = Left ("--lang must be " ++ showAvailLangs "or" ++ ".")
	| prec < 1 || prec > 60		= Left "--prec must be 1 < prec < 60."
	| not (checkTimeOk time)	= Left "--time must be given as HH:MM, where HH is in [0..23] and MM is in [0..59]."
	| style `notElem` [1, 2]	= Left "--style must be either 1 or 2 (see the man page)."
	| otherwise					= Right c

checkFTConf t@(TimerConf end lang now)
	| not (checkTimeOk end)
		&& end /= "unset"		= Left ("END must be given and in the same format as --time.")
	| not (checkTimeOk now)		= Left "--time must be given as HH:MM, where HH is in [0..23] and MM is in [0..59]."
	| otherwise					= Right t


-- | \[config] Check that a string is a properly formatted time (HH:MM).
checkTimeOk :: String -> Bool
checkTimeOk time = case break (== ':') time of
	(hh, _:mm)	->	not (null hh || null mm)
					&& all isDigit hh && all isDigit mm
					&& (let h = read hh; m = read mm
						in 0 <= h && h < 24 && 0 <= m && m < 60)
	_			->	False


-- main ===========================================================================================================================================================================


-- | There are two modes: telling the time, and setting the timer.
-- Telling the time happens when invoked as "fuzzytime clock". If the timer is set (if ~/.fuzzytimer exists), the countdown is shown. Otherwise, time is shown.
-- Setting the timer happens when invoked as "fuzzytime timer END", where END can be either the time to countdown to (as HH:MM), or "unset" to disable the timer.
main :: IO ()
main = do
	clockConf <- getDefClockConf
	timerConf <- getDefTimerConf
	conf <- cmdArgs (modes [clockConf, timerConf] &= program confHelpProgram &= summary confHelpSummary)
	case conf of
		-- | Showing the time mode.
		cc@(ClockConf _ _ _ _ _)
			->	do
				let realConf = if end timerConf == "empty" then
									conf
									else
									timerConf {
										  lang	= lang conf
										, now	= time conf
									}
				case checkFTConf realConf of
					Left e	-> putStrLn e
					Right c	-> print $ toFuzzyTime c
		-- | Setting the timer mode.
		tc@(TimerConf end _ _)
			->	do
				path <- confTimerFileLoc
				case checkFTConf tc of
					Left e	-> 	putStrLn e
					Right c	->	do
								removeFile path `catch` (\_ -> putStrLn "Timer is not set.")
								if end == "unset" then
									putStrLn "Timer has been unset."
									else do
									writeFile path end
									putStrLn $ "Timer has been set to " ++ end ++ "."
