{-# LANGUAGE DeriveDataTypeable #-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- |
---- Module			: Fuzzytime
---- Copyright		: (C) Kamil Stachowski <kamil.stachowski@gmail.com>
---- License		: GPL3+
---- Maintainer		: Kamil Stachowski <kamil.stachowski@gmail.com>
---- Stability		: unstable
---- Portability	: unportable
---- Prints the current time in a more casual way (the \"ten past six\"-style).
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

module Fuzzytime (
	-- * Description
	-- $description
	  main
	, FuzzyTime
	, toFuzzyTime
	) where

import System.Console.CmdArgs
import System.Time

-- $description
-- A small utility to print the current time in a more casual way (the \"ten past six\"-style).
--
-- The whole thing works like this: the current time is read and turned into CalendarTime. This is fuzzified (i.e. the precision is reduced) and at the same time changed into FuzzyTime. FuzzyTime holds the time as two Ints, and the target language. It is an instance of Show. Custom functions show FuzzyTime as the \"ten past six\"-style String.
--
-- Depends on N. Mitchell's System.Console.CmdArds 0.6.4-1
--
-- To add a new language, two things need to be done:
--
-- (1) The new language has to be added to the instance of Show FuzzyTime. (And to the help message.)
--
-- (2) An appropriate function has to be created to turn FuzzyTime -> String.


-- CHANGELOG
-- 0.1	2010.12.05
-- 		initial release: two languages (en and pl), 1 < precision < 60
-- 		0.1.1	2010.12.06
-- 				added cabal



-- config ===============================================================================


-- | \[config] The default language.
confDefaultLang :: String
confDefaultLang = "en"

-- | \[config] The default precision in minutes. Should be 1 < precision < 60.
confDefaultPrec :: Int
confDefaultPrec = 5


-- main =================================================================================


-- | The main part. Only reads the command line args and the current time, fuzzifies and prints it.
main :: IO ()
main = do
	cl <- cmdArgs clOpts
	now <- getClockTime >>= toCalendarTime
	print $ toFuzzyTime (prec cl) (lang cl) now
	

-- cl args ==============================================================================


-- | Two options can be set via the command line: language and precision.
data CLOpts = CLOpts {
	  lang :: String
	, prec :: Int
	} deriving (Show, Data, Typeable)

clOpts = CLOpts {
	  lang=confDefaultLang	&= help "Language (currently en and pl); default en."
	, prec=confDefaultPrec	&= help "Precision (1 < prec < 60 [minutes]); default 5."
	}
	&= program "fuzzytime"
	&= summary "Print fuzzy time, e.g. 10:52 -> ten to eleven.\nv0.1, 2010.05.12, kamil.stachowski@gmail.com, GPL3+"


-- FuzzyTime – main =====================================================================


-- | Data for fuzzified time. Only keeps hour, minutes (both as Ints), and language.
-- Can be created from CalendarTime with toFuzzyTime. The String output (the \"ten past six\"-style) is obtained through Show.
data FuzzyTime = FuzzyTime {
	  fzHour	:: Int
	, fzMin		:: Int
	, fzLang	:: String
	} deriving (Eq)

-- | This is where FuzzyTime Int Int String is turned into the time String.
instance Show FuzzyTime where
	show (FuzzyTime hour min lang) =
		case lang of
			"en" -> showFuzzyTimeEn hour min
			"pl" -> showFuzzyTimePl hour min
			otherwise -> "Language " ++ lang ++ " is not supported."


-- | Converts CalendarTime to FuzzyTime using the given precision. The language is set, too, for Show to know how to display it.
toFuzzyTime ::
	   Int			-- ^ precision
	-> String		-- ^ language
	-> CalendarTime	-- ^ time
	-> FuzzyTime
toFuzzyTime cPrec cLang time =
	if fuzzdM min /= 60 then
		FuzzyTime hour (fuzzdM min) cLang
	else
		FuzzyTime (hour+1) 0 cLang
	where
	min :: Int
	min = (ctMin time)
	hour :: Int
	hour = (ctHour time) `mod` 12
	fuzzdM :: Int -> Int
	fuzzdM m =
		let
			mf = fromIntegral m
			cf = fromIntegral cPrec
		in
			round(mf/cf) * cPrec


-- FuzzyTime – shows ====================================================================

-- English ------------------------------------------------------------------------------


showFuzzyTimeEn :: Int -> Int -> String
showFuzzyTimeEn hour min
	| min == 0	= numeralEn hour ++ " o’clock"
	| min <= 30	= numeralEn min ++ " past " ++ numeralEn hour
	| min > 30	= numeralEn (60-min) ++ " to " ++ numeralEn (hour+1)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."


numeralEn :: Int -> String
numeralEn n
	| n < 20			= numeralEnHelper1 n
	| n == 30			= "half"
	| n `mod` 10 == 0	= numeralEnHelper10 (n `div` 10)
	| otherwise			= numeralEnHelper10 (n `div` 10) ++ "-" ++ numeralEnHelper1 (n `mod` 10)
	where
	numeralEnHelper1 :: Int -> String
	numeralEnHelper1 i = ["twelve", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "quarter", "sixteen", "seventeen", "eighteen", "nineteen"] !! i
	numeralEnHelper10 :: Int -> String
	numeralEnHelper10 i = ["twenty", "thirty", "forty", "fifty"] !! (i-2)


-- Polish ------------------------------------------------------------------------------


showFuzzyTimePl :: Int -> Int -> String
showFuzzyTimePl hour min
	| min == 0	= numeralPlOrdNom hour
	| min < 30	= numeralPlCard min ++ " po " ++ numeralPlOrdPraep hour
	| min == 30	= "wpół do " ++ numeralPlOrdPraep (hour+1)
	| min > 30	= "za " ++ numeralPlCard (60-min) ++ " " ++ numeralPlOrdNom (hour+1)
	| otherwise	= "Oops, wychodzi, że jest " ++ show hour ++ ":" ++ show min ++ "."


numeralPlCard :: Int -> String
numeralPlCard n
	| n < 20			= numeralPlHelper1 n
	| n `mod` 10 == 0	= numeralPlHelper10 (n `div` 10)
	| otherwise			= numeralPlHelper10 (n `div` 10) ++ " " ++ numeralPlHelper1 (n `mod` 10)
	where
	numeralPlHelper1 :: Int -> String
	numeralPlHelper1 i = ["jeden", "dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć", "dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "kwadrans", "szesnaście", "siedemnaście", "osiemnaście", "dziewiętnaście"] !! (i-1)
	numeralPlHelper10 :: Int -> String
	numeralPlHelper10 i = ["dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt"] !! (i-2)

numeralPlOrdNom :: Int -> String
numeralPlOrdNom n = ["pierwsza", "druga", "trzecia", "czwarta", "piąta", "szósta", "siódma", "ósma", "dziewiąta", "dziesiąta", "jedenasta", "dwunasta"] !! (n-1)

numeralPlOrdPraep :: Int -> String
numeralPlOrdPraep n = ["dwunastej","pierwszej", "drugiej", "trzeciej", "czwartej", "piątej", "szóstej", "siódmej", "ósmej", "dziewiątej", "dziesiątej", "jedenastej", "dwunastej"] !! n
