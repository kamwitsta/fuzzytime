module FuzzyTime (
	  FuzzyTime (..)
	, toFuzzyTime
	, nextFTHour
	, FuzzyTimeConf (..)
	) where


import Data.Data


data FuzzyTime
	= FuzzyClock {
	  ftClock	:: Int
	, ftHour	:: Int
	, ftLang	:: String
	, ftMin		:: Int
	, ftNight	:: Bool
	, ftStyle	:: Int
	}
	| FuzzyTimer {
	  ftLang	:: String
	, ftMins	:: Int
	}


toFuzzyTime :: FuzzyTimeConf -> FuzzyTime

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
