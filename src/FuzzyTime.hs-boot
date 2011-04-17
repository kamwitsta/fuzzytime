module FuzzyTime (
	  FuzzyTime (..)
	, capsizeDef
	, toFuzzyTime
	, isTimerZero
	, nextFTHour
	, FuzzyTimeConf (..)
	, Time
	) where


import Data.Data ()


type Time = String

data FuzzyTime
	= FuzzyClock {
	  ftAm		:: Bool
	, ftCaps	:: Int
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


toFuzzyTime :: FuzzyTimeConf -> FuzzyTime

capsizeDef :: Int -> String -> String

isTimerZero :: FuzzyTime -> Bool

nextFTHour :: FuzzyTime -> Int


data FuzzyTimeConf
	= ClockConf {
	  caps	:: Int
	, clock	:: Int
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
