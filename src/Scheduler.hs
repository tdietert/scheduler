module Scheduler (
    schedule
  , available
  , meeting
  , hours
  , minutes
  , to 
) where

import Data.Time.Calendar as Cal
import Data.Time.Clock as Clock
import Data.Time.LocalTime as LocT

import Control.Applicative
import Control.Monad.Trans.Except 
import Data.List.Split
import Data.Map hiding (map)

type Name = String

type Scheduler = Except InputError
data InputError = DateFormatErr String
                | TimeFormatErr String

type Availability = Map Cal.Day [Duration]
data Person = Person Name Availability
data Duration = Duration Clock.NominalDiffTime Clock.NominalDiffTime
    deriving (Show)


-- janie's number:  850-322-3808
schedule = undefined

available :: Name -> Strin -> Scheduler Duration -> Scheduler Person
available name date dur = undefined

meeting = undefined

hours :: Integer -> Clock.NominalDiffTime
hours = realToFrac . Clock.secondsToDiffTime . (*3600) 

minutes :: Integer -> Clock.NominalDiffTime
minutes = realToFrac . Clock.secondsToDiffTime . (*60)
 
to :: String -> String -> Scheduler Duration
t1 `to` t2 = Duration <$> parseTime t1 <*> parseTime t2

parseTime :: String -> Scheduler Clock.NominalDiffTime 
parseTime t = 
    case map read (splitOn ":" t) of
        [h1,m1] -> let secs = h1*3600 + m1*60
                   in return . realToFrac $ Clock.secondsToDiffTime secs
        _       -> throwE $ TimeFormatErr (t ++ " arg to `to` must be in fmt HH:MM") 

parseDate :: String -> Scheduler Cal.Day
parseDate date = 
    case map read $ splitOn "-" date of
        [yr,mn,dy] -> return $ Cal.fromGregorian yr (fromIntegral mn) (fromIntegral dy)
        _          -> throwE $ DateFormatErr (date ++ " arg to `available must be in fmt ;YYYY-MM-DD")
