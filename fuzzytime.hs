{-# LANGUAGE DeriveDataTypeable #-}

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
	, FuzzyTime (..)
	, FuzzyTimeConf (..)
	, toFuzzyTime
	) where

import Data.Char (isDigit)
import System.Console.CmdArgs
import System.Time


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


-- main ===========================================================================================================================================================================


-- | The main part. Only reads the command line args and the current time, fuzzifies and prints it.
main :: IO ()
main = do
	conf <- cmdArgs =<< confSetCurrTime getFTConf		-- ^ confSetCurrTime required to avoid unsafePerformIO
	if checkFTConf conf /= "ok" then
		putStrLn $ checkFTConf conf
		else
		print $ toFuzzyTime conf


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
				

-- FuzzyTime – shows ==============================================================================================================================================================

-- Danish (by M_ller from bbs.archlinux.org, with my modifications) ---------------------------------------------------------------------------------------------------------------


showFuzzyTimeDa :: FuzzyTime -> String
showFuzzyTimeDa ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min < 30	= getMin min ++ " over " ++ getHour hour
	| min == 30	= "halv " ++ getHour (nextFTHour ft)
	| min > 30	= getMin (60-min) ++ " i " ++ getHour (nextFTHour ft)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
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


-- German -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeDe :: FuzzyTime -> String
showFuzzyTimeDe ft@(FuzzyTime clock hour _ min night style)
	| min == 0				= if getHour hour `elem` ["Mitternacht", "Mittag"] then getHour hour else getHour hour ++ " Uhr"
	| min `elem` [23..29]	
		&& style == 2		= getMin (30-min) ++ " vor halb " ++ getHour (nextFTHour ft)
	| min < 30				= getMin min ++ " nach " ++ getHour hour
	| min `elem` [31..37]
		&& style == 2		= getMin (min-30) ++ " nach halb " ++ getHour (nextFTHour ft)
	| min == 30				= "halb " ++ getHour (nextFTHour ft)
	| min > 30				= getMin (60-min) ++ " vor " ++ getHour (nextFTHour ft)
	| otherwise	= 			"Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralDe 12 else numeralDe h
								else
								if night then
									if min /=30  then "Mitternacht" else numeralDe clock
								else
									numeralDe 12
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
	numeralDeHelper1 i = ["ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwőlf", "dreizehn", "vierzehn", "fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"] !! (i-1)
	numeralDeHelper10 :: Int -> String
	numeralDeHelper10 i = ["zwanzig", "dreissig", "vierzig", "fünfzig"] !! (i-2)


-- Greek (thanks Gbak from bbs.archlinux.org) -------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeEl :: FuzzyTime -> String
showFuzzyTimeEl ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " και " ++ getMin min
	| min > 30	= getHour (nextFTHour ft) ++ " παρά " ++ getMin (60-min)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if clock==12 then numeralEl 12 else numeralEl h
		| otherwise			= numeralEl h
	getMin :: Int -> String
	getMin m
		| m == 30			= "μισή"
		| m `elem` [15, 45]	= "τέταρτο"
		| otherwise			= numeralEl m


numeralEl :: Int -> String
numeralEl n
	| n < 20			= numeralElHelper1 n
	| n `mod` 10 == 0	= numeralElHelper10 (n `div` 10)
	| otherwise			= numeralElHelper10 (n `div` 10) ++ " " ++ numeralElHelper1 (n `mod` 10)
	where
	numeralElHelper1 :: Int -> String
	numeralElHelper1 i = ["ένας", "δυο", "τρεις", "τέσσερις", "πέντε", "έξι", "εφτά", "οχτώ", "εννιά", "δέκα", "έντεκα", "δώδεκα", "δεκατρείς", "δεκατέσσερις", "δεκαπέντε", "δεκάξι", "δεκαεφτά", "δεκαοχτώ", "δεκαεννιά"] !! (i-1)
	numeralElHelper10 :: Int -> String
	numeralElHelper10 i = ["είκοσι", "τριάντα", "σαράντα", "πενήντα"] !! (i-2)


-- Dutch (thanks litemotiv from bbs.archlinux.org)---------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeNl :: FuzzyTime -> String
showFuzzyTimeNl ft@(FuzzyTime clock hour _ min night style)
	| min == 0				= if getHour hour == "middernacht" then getHour hour else getHour hour ++ " uur"
	| min `elem` [20..29]	
		&& style == 2		= getMin (30-min) ++ " voor half " ++ getHour (nextFTHour ft)
	| min < 30				= getMin min ++ " over " ++ getHour hour
	| min `elem` [31..40]
		&& style == 2		= getMin (min-30) ++ " over half " ++ getHour (nextFTHour ft)
	| min == 30				= "half " ++ getHour (nextFTHour ft)
	| min > 30				= getMin (60-min) ++ " voor " ++ getHour (nextFTHour ft)
	| otherwise	= 			"Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: Int -> String
	getHour h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralNl 12 else numeralNl h
								else
								if night then
									if min /= 30 then "middernacht" else numeralNl clock
								else
									numeralNl 12
		| otherwise			= numeralNl h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "kwart"
		| otherwise			= numeralNl m


numeralNl :: Int -> String
numeralNl n
	| n < 20			= numeralNlHelper1 n
	| n `mod` 10 == 0	= numeralNlHelper10 (n `div` 10)
	| otherwise			= numeralNlHelper1 (n `mod` 10) ++
							(if (n `mod` 10) `elem` [2, 3] then "ën" else "en") ++
							numeralNlHelper10 (n `div` 10)
	where
	numeralNlHelper1 :: Int -> String
	numeralNlHelper1 i = ["een", "twee", "drie", "vier", "vijf", "zes", "zeven", "acht", "negen", "tien", "elf", "twaalf", "dertien", "veertien", "vijftien", "zestien", "zeventien", "achttien", "negentien"] !! (i-1)
	numeralNlHelper10 :: Int -> String
	numeralNlHelper10 i = ["twintig", "dertig", "veertig", "vijftig"] !! (i-2)


-- English ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeEn :: FuzzyTime -> String
showFuzzyTimeEn ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= if getHour hour `elem` ["midnight", "noon"] then getHour hour else getHour hour ++ " o’clock"
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


-- French -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeFr :: FuzzyTime -> String
showFuzzyTimeFr ft@(FuzzyTime clock hour _ min night style)
	| min == 0	= getHour hour
	| min <= 30	= getHour hour ++ " " ++ getMin min
	| min > 30	= getHour (nextFTHour ft) ++ " moins " ++ getMin (60-min)
	| otherwise	= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
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
	numeralFrHelper1 i = ["une", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze", "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf"] !! (i-1)
	numeralFrHelper10 :: Int -> String
	numeralFrHelper10 i = ["vingt", "trente", "quarante", "cinquante"] !! (i-2)


-- Polish -------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimePl :: FuzzyTime -> String
showFuzzyTimePl ft@(FuzzyTime clock hour _ min night style)
	| min == 0			= getHourEven hour
	| min `elem` [23..29]
		&& style == 2	= "za " ++ getMin (30-min) ++ " w pół do " ++ getHourOdd (nextFTHour ft)
	| min < 30			= getMin min ++ " po " ++ getHourOdd hour
	| min == 30			= "w pół do " ++ getHourOdd (nextFTHour ft)
	| min `elem` [31..37]
		&& style == 2	= getMin (min-30) ++ " po w pół do " ++ getHourOdd (nextFTHour ft)
	| min > 30			= "za " ++ getMin (60-min) ++ " " ++ getHourEven (nextFTHour ft)
	| otherwise			= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHourEven :: Int -> String
	getHourEven h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrd "Nom" 12 else numeralPlOrd "Nom" h
								else
								if night then "północ" else numeralPlOrd "Nom" h
		| otherwise			= numeralPlOrd "Nom" h
	getHourOdd :: Int -> String
	getHourOdd h
		| h `mod` 12 == 0	= if style==1 then
								if clock==12 then numeralPlOrd "Praep" 12 else numeralPlOrd "Praep" h
								else
								if night then
									if min < 30 then "północy" else numeralPlOrd "Praep" clock
								else
									numeralPlOrd "Praep" 12
		| otherwise			= numeralPlOrd "Praep" h
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

numeralPlOrd :: String -> Int -> String
numeralPlOrd c n
	| n <= 20			= numeralPlOrdHelper1 c n
	| otherwise			= numeralPlOrdHelper10 c ++ " " ++ numeralPlOrdHelper1 c (n `mod` 10)
	where
	numeralPlOrdHelper1 :: String -> Int -> String
	numeralPlOrdHelper1 "Nom" n = ["pierwsza", "druga", "trzecia", "czwarta", "piąta", "szósta", "siódma", "ósma", "dziewiąta", "dziesiąta", "jedenasta", "dwunasta", "trzynasta", "czternasta", "piętnasta", "szesnasta", "siedemnasta", "osiemnasta", "dziewiętnasta", "dwudziesta"] !! (n-1)
	numeralPlOrdHelper1 "Praep" n = ["pierwszej", "drugiej", "trzeciej", "czwartej", "piątej", "szóstej", "siódmej", "ósmej", "dziewiątej", "dziesiątej", "jedenastej", "dwunastej", "trzynastej", "czternastej", "piętnastej", "szesnastej", "siedemnastej", "osiemnastej", "dziewiętnastej", "dwudziestej"] !! (n-1)
	numeralPlOrdHelper10 :: String -> String
	numeralPlOrdHelper10 "Nom" = "dwudziesta"
	numeralPlOrdHelper10 "Praep" = "dwudziestej"


-- Turkish ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


showFuzzyTimeTr :: FuzzyTime -> String
showFuzzyTimeTr ft@(FuzzyTime clock hour _ min night style)
	| min == 0			= "saat " ++ getHour "Nom" hour
	| min `elem` [22..29]
		&& style == 2	= getHour "Nom" hour ++ " buçuğa " ++ getMin (30-min) ++ " var"
	| min < 30			= getHour "Acc" hour ++ " " ++ getMin min ++ " geçiyor"
	| min == 30			= if hour `mod` 12 == 0 then "yarım" else getHour "Nom" hour ++ " buçuk"
	| min `elem` [31..38]
		&& style == 2	= getHour "Nom" hour ++ " buçuğu " ++ getMin (min-30) ++ " geçiyor"
	| min > 30			= getHour "Dat" (nextFTHour ft) ++ " " ++ getMin (60-min) ++ " var"
	| otherwise			= "Oops, it looks like it's " ++ show hour ++ ":" ++ show min ++ "."
	where
	getHour :: String -> Int -> String
	getHour c h
		| h `mod` 12 == 0	= if clock==12 then numeralTr c 12 else numeralTr c h
		| otherwise			= numeralTr c h
	getMin :: Int -> String
	getMin m
		| m `elem` [15, 45]	= "çeyrek"
		| otherwise			= numeralTr "Nom" m


numeralTr :: String -> Int -> String
numeralTr c n
	| n < 10			= numeralTrHelper1 c n
	| n `mod` 10 == 0	= numeralTrHelper10 c (n `div` 10)
	| otherwise			= numeralTrHelper10 "Nom" (n `div` 10) ++ " " ++ numeralTrHelper1 c (n `mod` 10)
	where
	numeralTrHelper1 :: String -> Int -> String
	numeralTrHelper1 "Nom" i = ["bir", "iki", "üç", "dört", "beş", "altı", "yedi", "sekiz", "dokuz"] !! (i-1)
	numeralTrHelper1 "Dat" i = ["bire", "ikiye", "üçe", "dörde", "beşe", "altıya", "yediye", "sekize", "dokuza"] !! (i-1)
	numeralTrHelper1 "Acc" i = ["biri", "ikiyi", "üçü", "dördü", "beşi", "altıyı", "yediyi", "sekizi", "dokuzu"] !! (i-1)
	numeralTrHelper10 :: String -> Int -> String
	numeralTrHelper10 "Nom" i = ["on", "yirmi", "otuz", "kırk", "elli"] !! (i-1)
	numeralTrHelper10 "Dat" i = ["ona", "yirmiye", "otuza", "kırka", "elliye"] !! (i-1)
	numeralTrHelper10 "Acc" i = ["onu", "yirmiyi", "otuzu", "kırkı", "elliyi"] !! (i-1)
