module FuzzyTime (
	  FuzzyTime (..)
	, toFuzzyTime
	, isTimerZero
	, nextFTHour
	, FuzzyTimeConf (..)
	, Time
	) where


import Data.Data


type Time = String

data FuzzyTime
	= FuzzyClock {
	  ftAm		:: Bool
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

isTimerZero :: FuzzyTime -> Bool

nextFTHour :: FuzzyTime -> Int


data FuzzyTimeConf
	= ClockConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	}
	| TimerConf {
	  end	:: String
	, lang	:: String
	, now	:: String
	}
