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

import System.Console.CmdArgs
import System.Time

import FuzzyTime


-- $description
-- A small utility to tell the current time in a more familiar way (the \"ten past six\"-style).
--
-- The whole thing works like this: the current time is read and turned into CalendarTime. This is fuzzified (i.e. the precision is reduced) and at the same time changed into FuzzyTime. FuzzyTime holds the time as two Ints (hour and min), the target language, the desired clock (12 vs. 24) and an information on whether it is am or pm (needed for the 12-hour clock). Custom functions show FuzzyTime as the \"ten past six\"-style String.
--
-- Depends on N. Mitchell's System.Console.CmdArds 0.6.4-1
--
-- To add a new language, two things need to be done:
--
-- (1) The new language has to be added to checkFTConf, to the instance of Show FuzzyTime, and to the help message.
--
-- (2) An appropriate function has to be created to turn FuzzyTime -> String.


-- CHANGELOG
-- TODO
-- 		exit codes
-- 		answers for Greek (:35, midnight, εντεκάμιση)
--		check Danish: midnight + noon, halves
-- 0.5
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




-- main ===========================================================================================================================================================================


-- | The main part. Only reads the command line args and the current time, fuzzifies and prints it.
main :: IO ()
main = do
	-- conf <- cmdArgs =<< confSetCurrTime getFTConf		-- ^ confSetCurrTime required to avoid unsafePerformIO
	conf <- cmdArgs getFTConf
	if checkFTConf conf /= "ok" then
		putStrLn $ checkFTConf conf
		else
		print $ toFuzzyTime conf


				

-- FuzzyTime – shows ==============================================================================================================================================================















