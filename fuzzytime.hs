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

module Main (
	-- * Description
	-- $description
	  main
	, FuzzyTime
	, FuzzyTimeConf
	, toFuzzyTime
	) where

import System.Console.CmdArgs
import System.Time


-- $description
-- A small utility to print the current time in a more casual way (the \"ten past six\"-style).
--
-- The whole thing works like this: the current time is read and turned into CalendarTime. This is fuzzified (i.e. the precision is reduced) and at the same time changed into FuzzyTime. FuzzyTime holds the time as two Ints (hour and min), the target language, the desired clock (12 vs. 24) and an information on whether it is am or pm (needed for the 12-hour clock). Custom functions show FuzzyTime as the \"ten past six\"-style String.
--
-- Depends on N. Mitchell's System.Console.CmdArds 0.6.4-1
--
-- To add a new language, two things need to be done:
--
-- (1) The new language has to be added to FuzzyTimeConf, to the instance of Show FuzzyTime, and to the help message.
--
-- (2) An appropriate function has to be created to turn FuzzyTime -> String.


-- CHANGELOG
-- TODO
-- 		more languages
-- 		exit codes
-- 0.3	2011.01.14
-- 		added midnight and noon
-- 		added checking cli options
-- 		fixed the "quarter past quarter" bug
-- 0.2	2011.01.12
-- 		added French and German
-- 		added 12 vs. 24-hour clock
-- 0.1	2010.12.05
-- 		initial release: two languages (en and pl), 1 < precision < 60
-- 		0.1.1	2010.12.06
-- 				added cabal



-- config ===============================================================================


-- | \[config] The default for 12 vs. 24-hour clock.
confDefaultClock :: Int
confDefaultClock = 12

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
	conf <- cmdArgs ftConf
	if (checkConf conf) /= "ok" then
		putStrLn $ checkConf conf
		else do
		calTime <- getClockTime >>= toCalendarTime
		print $ toFuzzyTime conf calTime


-- cl args ==============================================================================


-- | Three options can be set via the command line: clock (12 vs. 24), language and precision.
data FuzzyTimeConf = FuzzyTimeConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	} deriving (Show, Data, Typeable)

ftConf :: FuzzyTimeConf
ftConf = FuzzyTimeConf {
	  clock	=confDefaultClock	&= help "12 or 24-hour clock; default 12-hour."
	, lang	=confDefaultLang	&= help "Language (currently de, en, fr and pl); default en."
	, prec	=confDefaultPrec	&= help "Precision (1 < prec < 60 [minutes]); default 5."
	}
	&= program "fuzzytime"
	&= summary "Print fuzzy time, e.g. 10:52 -> ten to eleven.\nv0.2, 2011.01.12, kamil.stachowski@gmail.com, GPL3+"


checkConf :: FuzzyTimeConf -> String
checkConf (FuzzyTimeConf clock lang prec)
	| not (clock `elem` [12, 24])	= "--clock must be either 12 or 24."
	| not (lang `elem` ["de", "en", "fr", "pl"])	= "--lang must be de, en, fr or pl."
	| prec < 1 || prec > 60			= "--prec must be 1 < prec < 60."
	| otherwise						= "ok"


-- FuzzyTime – main =====================================================================


-- | Data for fuzzified time. Only keeps hour, minutes (both as Ints), am (vs. pm), clock (12 vs. 24) and language.
-- Can be created from CalendarTime with toFuzzyTime. The String output (the \"ten past six\"-style) is obtained through Show.
data FuzzyTime = FuzzyTime {
	  fzAm		:: Bool
	, fzClock	:: Int
	, fzHour	:: Int
	, fzLang	:: String
	, fzMin		:: Int
	} deriving (Eq)

-- | This is where FuzzyTime Int Int String is turned into the time String.
-- It is assumed that by the time these functions are called, hour will be in [0..23] and min will be in [0..59].
instance Show FuzzyTime where
	show ft@(FuzzyTime _ _ _ lang _) = case lang of
		"de" -> showFuzzyTimeDe ft
		"en" -> showFuzzyTimeEn ft
		"fr" -> showFuzzyTimeFr ft
		"pl" -> showFuzzyTimePl ft
		otherwise -> "Language " ++ lang ++ " is not supported."


-- | Converts CalendarTime to FuzzyTime using the given precision. The language and clock are also set, so that Show knows how to display it.
toFuzzyTime :: FuzzyTimeConf -> CalendarTime -> FuzzyTime
toFuzzyTime (FuzzyTimeConf cClock cLang cPrec) (CalendarTime _ _ _ hour min _ _ _ _ _ _ _) =
	FuzzyTime (hour < 12) cClock fuzzdHour cLang (fuzzdMin min) 
	where
	fuzzdHour :: Int
	fuzzdHour =
		if cClock==24 then
			hour
			else
			hour `mod` 12
	fuzzdMin :: Int -> Int
	fuzzdMin m =
		let
			mf = fromIntegral m
			cf = fromIntegral cPrec
		in
			round(mf/cf) * cPrec


-- FuzzyTime – shows ====================================================================

-- English ------------------------------------------------------------------------------


showFuzzyTimeEn :: FuzzyTime -> String
showFuzzyTimeEn (FuzzyTime am clock hour _ min)
	| min == 0	= if (getHour hour) `elem` ["midnight", "noon"] then (getHour hour) else (getHour hour) ++ " o’clock"
	| min <= 30	= getMin min ++ " past " ++ getHour hour
	| min > 30	= getMin (60-min) ++ " to " ++ getHour (hour+1)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h == 0 && am		= "midnight"
		| h == 0 && not am	= if clock==12 then "noon" else "midnight"
		| h == 12			= "noon"
		| otherwise			= numeralEn h
	getMin :: Int -> String
	getMin m
		| m == 30			= "half"
		| m `elem` [15, 45]	= "quarter"
		| otherwise			= numeralEn m


numeralEn :: Int -> String
numeralEn n
	| n < 20			= numeralEnHelper1 n
	| n `mod` 10 == 0	= numeralEnHelper10 (n `div` 10)
	| otherwise			= numeralEnHelper10 (n `div` 10) ++ "-" ++ numeralEnHelper1 (n `mod` 10)
	where
	numeralEnHelper1 :: Int -> String
	numeralEnHelper1 i = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! i
	numeralEnHelper10 :: Int -> String
	numeralEnHelper10 i = ["twenty", "thirty", "forty", "fifty"] !! (i-2)


-- German -------------------------------------------------------------------------------


showFuzzyTimeDe :: FuzzyTime -> String
showFuzzyTimeDe (FuzzyTime am clock hour _ min)
	| min == 0	= if (getHour hour) `elem` ["Mitternacht", "Mittag"] then (getHour hour) else (getHour hour) ++ " Uhr"
	| min < 30	= getMin min ++ " nach " ++ getHour hour
	| min == 30	= "halb " ++ getHour (hour+1)
	| min > 30	= getMin (60-min) ++ " vor " ++ getHour (hour+1)
	| otherwise	= "Oops, es sieht aus, dass es " ++ show hour ++ ":" ++ show min ++ " ist."
	where
	getHour :: Int -> String
	getHour h
		| h == 0 && am		= "Mitternacht"
		| h == 0 && not am	= if clock==12 then "Mittag" else "Mitternacht"
		| h == 12			= "Mittag"
		| otherwise			= numeralDe h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "Viertel"
		| otherwise			= numeralDe m


numeralDe :: Int -> String
numeralDe n
	| n < 20			= numeralDeHelper1 n
	| n `mod` 10 == 0	= numeralDeHelper10 (n `div` 10)
	| otherwise			= numeralDeHelper1 (n `mod` 10) ++ "und" ++ numeralDeHelper10 (n `div` 10)
	where
	numeralDeHelper1 :: Int -> String
	numeralDeHelper1 i = ["null", "ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwőlf", "dreizehn", "vierzehn", "fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"] !! i
	numeralDeHelper10 :: Int -> String
	numeralDeHelper10 i = ["zwanzig", "dreissig", "vierzig", "fünfzig"] !! (i-2)


-- French -------------------------------------------------------------------------------


showFuzzyTimeFr :: FuzzyTime -> String
showFuzzyTimeFr (FuzzyTime am clock hour _ min)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " " ++ getMin min
	| min > 30	= getHour (hour+1) ++ " moins " ++ getMin (60-min)
	| otherwise	= "Oops, il semble qu’il est " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h == 0 && am		= "minuit"
		| h == 0 && not am	= if clock==12 then "midi" else "minuit"
		| h == 12			= "midi"
		| otherwise			= numeralFr h ++ if h==1 then " heure" else " heures"
	getHourWord :: Int -> String
	getHourWord h = if h==1 then " heure" else " heures"
	getMin :: Int -> String
	getMin m
		| min == 15			= "et quart"
		| min == 30			= "et demie"
		| min == 45			= "le quart"
		| otherwise			= numeralFr m


numeralFr :: Int -> String
numeralFr n
	| n < 20			= numeralFrHelper1 n
	| n `mod` 10 == 0	= numeralFrHelper10 (n `div` 10)
	| otherwise			= numeralFrHelper10 (n `div` 10) ++ "-" ++ numeralFrHelper1 (n `mod` 10)
	where
	numeralFrHelper1 :: Int -> String
	numeralFrHelper1 i = ["douze", "une", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf"] !! i
	numeralFrHelper10 :: Int -> String
	numeralFrHelper10 i = ["vingt", "trente", "quarante", "cinquante"] !! (i-2)


-- Polish ------------------------------------------------------------------------------


showFuzzyTimePl :: FuzzyTime -> String
showFuzzyTimePl (FuzzyTime am clock hour _ min)
	| min == 0	= getHourEven hour
	| min < 30	= getMin min ++ " po " ++ getHourUneven hour
	| min == 30	= "wpół do " ++ getHourUneven (hour+1)
	| min > 30	= "za " ++ getMin (60-min) ++ " " ++ getHourEven (hour+1)
	| otherwise	= "Oops, wychodzi, że jest " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHourEven :: Int -> String
	getHourEven h
		| h == 0 && am		= "północ"
		| h == 0 && not am	= if clock==12 then "południe" else "północ"
		| h == 12			= "południe"
		| otherwise			= numeralPlOrdNom h
	getHourUneven :: Int -> String
	getHourUneven h
		| h == 0 && am		= "północy"
		| h == 0 && not am	= if clock==12 then "południu" else "północy"
		| h == 12			= "południu"
		| otherwise			= numeralPlOrdPraep h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwadrans"
		| otherwise			= numeralPlCard m


numeralPlCard :: Int -> String
numeralPlCard n
	| n < 20			= numeralPlCardHelper1 n
	| n `mod` 10 == 0	= numeralPlCardHelper10 (n `div` 10)
	| otherwise			= numeralPlCardHelper10 (n `div` 10) ++ " " ++ numeralPlCardHelper1 (n `mod` 10)
	where
	numeralPlCardHelper1 :: Int -> String
	numeralPlCardHelper1 i = ["jeden", "dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć", "dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "kwadrans", "szesnaście", "siedemnaście", "osiemnaście", "dziewiętnaście"] !! (i-1)
	numeralPlCardHelper10 :: Int -> String
	numeralPlCardHelper10 i = ["dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt"] !! (i-2)

numeralPlOrdNom :: Int -> String
numeralPlOrdNom n
	| n <= 20			= numeralPlOrdNomHelper1 n
	| otherwise			= "dwudziesta " ++ numeralPlOrdNomHelper1 (n `mod` 10)
	where
	numeralPlOrdNomHelper1 :: Int -> String
	numeralPlOrdNomHelper1 n = ["dwunasta", "pierwsza", "druga", "trzecia", "czwarta", "piąta", "szósta", "siódma", "ósma", "dziewiąta", "dziesiąta", "jedenasta", "dwunasta", "trzynasta", "czternasta", "piętnasta", "szesnasta", "siedemnasta", "osiemnasta", "dziewiętnasta", "dwudziesta"] !! n

numeralPlOrdPraep :: Int -> String
numeralPlOrdPraep n
	| n <= 20			= numeralPlOrdPraepHelper1 n
	| otherwise			= "dwudziestej " ++ numeralPlOrdPraepHelper1 (n `mod` 10)
	where
	numeralPlOrdPraepHelper1 :: Int -> String
	numeralPlOrdPraepHelper1 n = ["dwunastej","pierwszej", "drugiej", "trzeciej", "czwartej", "piątej", "szóstej", "siódmej", "ósmej", "dziewiątej", "dziesiątej", "jedenastej", "dwunastej", "trzynastej", "czternastej", "piętnastej", "szesnastej", "siedemnastej", "osiemnastej", "dziewiętnastej", "dwudziestej"] !! n
