module FuzzyTime (
	  FuzzyTime (..)
	, FuzzyTimeConf (..)
	, checkFTConf
	, getFTConf
	, nextFTHour
	, toFuzzyTime
	) where


import Data.Char (isDigit)
import System.Console.CmdArgs
import System.Time

data FuzzyTimeConf = FuzzyTimeConf {
	  clock	:: Int
	, lang	:: String
	, prec	:: Int
	, time	:: String
	, style	:: Int
	}
data FuzzyTime = FuzzyTime {
	  fzClock	:: Int
	, fzHour	:: Int
	, fzLang	:: String
	, fzMin		:: Int
	, fzNight	:: Bool
	, fzStyle	:: Int
	}

checkFTConf :: FuzzyTimeConf -> String

getFTConf :: FuzzyTimeConf
toFuzzyTime :: FuzzyTimeConf -> FuzzyTime
nextFTHour :: FuzzyTime -> Int
