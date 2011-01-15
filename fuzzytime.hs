{-# LANGUAGE DeriveDataTypeable #-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
---- |
---- Module			: Fuzzytime
---- Copyright		: (C) Kamil Stachowski <kamil.stachowski@gmail.com>
---- License		: GPL3+
---- Maintainer		: Kamil Stachowski <kamil.stachowski@gmail.com>
---- Stability		: unstable
---- Portability	: unportable
---- Tells the time in a more humane way (the \"ten past six\"-style).
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
-- A small utility to tell the current time in a more humane way (the \"ten past six\"-style).
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
-- 		more languages
-- 		exit codes
-- 		styles
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
-- 0.1	2010.12.05
-- 		initial release: two languages (en and pl), 1 < precision < 60
-- 		0.1.1	2010.12.06
-- 				added cabal


-- config ===============================================================================


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


-- main =================================================================================


-- | The main part. Only reads the command line args and the current time, fuzzifies and prints it.
main :: IO ()
main = do
	fullFTConf <- confSetCurrTime getFTConf		-- ^ required to avoid unsafePerformIO
	conf <- cmdArgs fullFTConf
	if (checkFTConf conf) /= "ok" then
		putStrLn $ checkFTConf conf
		else
		print $ toFuzzyTime conf


-- cl args ==============================================================================


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
	| not (clock `elem` [12, 24])	= "--clock must be either 12 or 24."
	| not (lang `elem` ["da", "de", "en", "fr", "pl"])	= "--lang must be de, en, fr or pl."
	| prec < 1 || prec > 60			= "--prec must be 1 < prec < 60."
	| not checkTimeOk				= "--time must be given as HH:MM, where HH is in [0..23] and MM is in [0..59]"
	| not (style `elem` [1, 2])		= "--style must be either 1 or 2 (see the man page)."
	| otherwise						= "ok"
	where
	checkTimeOk :: Bool
	checkTimeOk = ':' `elem` time && h `elem` [0..23] && m `elem` [0..59]
		where
		h = read $ takeWhile (/=':') time 
		m = read $ reverse . takeWhile (/=':') . reverse $ time


-- | Fill the config with the default values.
getFTConf :: FuzzyTimeConf
getFTConf = FuzzyTimeConf {
	  clock	= confDefClock	&= help "12 or 24-hour clock; default 12-hour."
	, lang	= confDefLang	&= help "Language (currently da, de, en, fr and pl); default en."
	, prec	= confDefPrec	&= help "Precision (1 < prec < 60 [minutes]); default 5."
	, time	= ""			&= help "Time to fuzzify as HH:MM; default current time."			-- ^ time is set via confSetCurrTime to avoid unsafePerformIO
	, style	= confDefStyle	&= help "How the time is told (seem the man page); default 1."
	}
	&= program "fuzzytime"
	&= summary "Tell the time in a more humane way, e.g. 10:52 -> ten to eleven.\nv0.4, 2011.01.15, kamil.stachowski@gmail.com, GPL3+"


-- FuzzyTime – main =====================================================================


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
		"en" -> showFuzzyTimeEn ft
		"fr" -> showFuzzyTimeFr ft
		"pl" -> showFuzzyTimePl ft
		otherwise -> "Language " ++ lang ++ " is not supported."


-- | Converts CalendarTime to FuzzyTime using the given precision. The language and clock are also set, so that Show knows how to display it.
toFuzzyTime :: FuzzyTimeConf -> FuzzyTime
toFuzzyTime (FuzzyTimeConf cClock cLang cPrec cTime cStyle) =
	FuzzyTime cClock fuzzdHour cLang (fuzzdMin min) (hour < 10 || hour > 22) cStyle
	where
	hour :: Int
	hour = read $ takeWhile (/=':') cTime
	min :: Int
	min = read $ reverse . takeWhile (/=':') . reverse $ cTime
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
	| otherwise						= hour + 1
				

-- FuzzyTime – shows ====================================================================

-- Danish (by M_ller with my modifications) ---------------------------------------------


showFuzzyTimeDa :: FuzzyTime -> String
showFuzzyTimeDa ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min < 30	= getMin min ++ " over " ++ getHour hour
	| min == 30	= "halv " ++ getHour (nextFTHour ft)
	| min > 30	= getMin (60-min) ++ " i " ++ getHour (nextFTHour ft)
	| otherwise	= "Ups, det ser ud til den er " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralDa 12 else numeralDa h
								else
								if night then
									"midnat"
									else
									if min `elem` [0, 30] then "aften" else numeralDa h
		| otherwise			= numeralDa h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kvart"
		| otherwise 		= numeralDa m


numeralDa :: Int -> String
numeralDa n
	| n < 20			= numeralDaHelper1 n
	| n `mod` 10 == 0	= numeralDaHelper10 (n `div` 10)
	| otherwise			= numeralDaHelper1 (n `mod` 10) ++ "og" ++ numeralDaHelper10 (n `div` 10)
	where
	numeralDaHelper1 :: Int -> String
	numeralDaHelper1 i = ["en", "to", "tre", "fire", "fem", "seks", "syv", "otte", "ni", "ti", "elleve", "tolv", "tretten", "fjorten", "femten", "seksten", "sytten", "atten", "nitten"] !! (i-1)
	numeralDaHelper10 :: Int -> String
	numeralDaHelper10 i = ["tyve", "tredive", "fyrre", "halvtreds"] !! (i-2)


-- English ------------------------------------------------------------------------------


showFuzzyTimeEn :: FuzzyTime -> String
showFuzzyTimeEn ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= if (getHour hour) `elem` ["midnight", "noon"] then (getHour hour) else (getHour hour) ++ " o’clock"
	| min <= 30	= getMin min ++ " past " ++ getHour hour
	| min > 30	= getMin (60-min) ++ " to " ++ getHour (nextFTHour ft)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralEn 12 else numeralEn h
								else
								if night then "midnight" else "noon"
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
	numeralEnHelper1 i = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! (i-1)
	numeralEnHelper10 :: Int -> String
	numeralEnHelper10 i = ["twenty", "thirty", "forty", "fifty"] !! (i-2)


-- German -------------------------------------------------------------------------------


showFuzzyTimeDe :: FuzzyTime -> String
showFuzzyTimeDe ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= if (getHour hour) `elem` ["Mitternacht", "Mittag"] then (getHour hour) else (getHour hour) ++ " Uhr"
	| min < 30	= getMin min ++ " nach " ++ getHour hour
	| min == 30	= "halb " ++ getHour (nextFTHour ft)
	| min > 30	= getMin (60-min) ++ " vor " ++ getHour (nextFTHour ft)
	| otherwise	= "Oops, es sieht aus, dass es " ++ show hour ++ ":" ++ show min ++ " ist."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralDe 12 else numeralDe h
								else
								if night then
									"Mitternacht"
									else
									if min `elem` [0, 30] then "Mittag" else numeralDe h
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
showFuzzyTimeFr ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " " ++ getMin min
	| min > 30	= getHour (nextFTHour ft) ++ " moins " ++ getMin (60-min)
	| otherwise	= "Oops, il semble qu’il est " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								(if clock==12 then numeralFr 12 else numeralFr h) ++ getHourWord h
								else
								if night then "minuit" else "midi"
		| otherwise			= numeralFr h ++ getHourWord h
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
showFuzzyTimePl ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHourEven hour
	| min < 30	= getMin min ++ " po " ++ getHourOdd hour
	| min == 30	= "wpół do " ++ getHourOdd (nextFTHour ft)
	| min > 30	= "za " ++ getMin (60-min) ++ " " ++ getHourEven (nextFTHour ft)
	| otherwise	= "Oops, wychodzi, że jest " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHourEven :: Int -> String
	getHourEven h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrdNom 12 else numeralPlOrdNom h
								else
								if night then "północ" else numeralPlOrdNom h
		| otherwise			= numeralPlOrdNom h
	getHourOdd :: Int -> String
	getHourOdd h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrdPraep 12 else numeralPlOrdPraep h
								else
								if night && min < 30 then "północy" else numeralPlOrdPraep h
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
