module FuzzyTime (
	  FuzzyTime (..)
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
	  cClock	:: Int
	, cLang	:: String
	, cPrec	:: Int
	, cTime	:: Time
	, cStyle	:: Int
	}
	| TimerConf {
	  cEnd	:: Time
	, cLang	:: String
	, cNow	:: Time
	}
