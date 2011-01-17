module FuzzyTime (
	  FuzzyTime (..)
	, toFuzzyTime
	, nextFTHour
	, FuzzyTimeConf (..)
	) where


import Data.Data


data FuzzyTime = FuzzyTime {
	  fzClock	:: Int
	, fzHour	:: Int
	, fzLang	:: String
	, fzMin		:: Int
	, fzNight	:: Bool
	, fzStyle	:: Int
	}

toFuzzyTime :: FuzzyTimeConf -> FuzzyTime

nextFTHour :: FuzzyTime -> Int


data FuzzyTimeConf = FuzzyTimeConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	}
