module Utils where

import Data.Time.Calendar.OrdinalDate
import Data.Time


parseDate :: String -> Day
parseDate dateAsString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateAsString :: Day

parseTimestamp :: String -> LocalTime
parseTimestamp timestampStr = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestampStr :: LocalTime

parseTime :: String -> TimeOfDay
parseTime timeStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" timeStr :: TimeOfDay

-- Time formats: https://www.stackage.org/haddock/nightly-2023-12-09/time-1.12.2/Data-Time-Format-Internal.html#g:2