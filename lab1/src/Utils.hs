module Utils where

import Data.Time.Calendar.OrdinalDate
import Data.Time


parseDate :: String -> Day
parseDate dateAsString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateAsString :: Day