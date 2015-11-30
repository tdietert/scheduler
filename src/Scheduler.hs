module Scheduler (
    schedule
  , available
  , meeting
  , hours
  , minutes
  , to
  , test 
) where

-- janie's number:  850-322-3808
import Data.Time.Calendar as Cal
import Data.Time.Clock as Clock
import Data.Time.LocalTime as LocT

import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Writer
import Control.Monad

import Data.List.Split
import Data.Map hiding (map,foldl)
import Data.Maybe

type Name = String

--------------------
test :: [Scheduler ()] 
test = [
      available "Hans" "2015-10-26" ("10:00" `to` "12:00") 
    , available "Hans"   "2015-10-26" ("13:00" `to` "17:00")
    , available "Hans"   "2015-10-28" ("13:00" `to` "17:00")
    , available "Philip" "2015-10-26" ("09:30" `to` "17:00")
    , available "Philip" "2015-10-28" ("09:30" `to` "12:00")
    , available "Tom"    "2015-10-26" ("09:30" `to` "16:00")
    , available "Tom"    "2015-10-27" ("09:30" `to` "16:00")
    , meeting ["Philip", "Hans", "Tom"] (hours 1 + minutes 15) "2015-10-26" "2015-10-30"
    , meeting ["Hans", "Tom"] (hours 1) "2015-10-26" "2015-10-30" 
  ]
--------------------

data InputError = DateFormatErr String
                | TimeFormatErr String
    deriving (Show)

type Availability = Map Cal.Day [Duration]
type Employees = Map Name Availability
data Duration = Duration Clock.NominalDiffTime Clock.NominalDiffTime
    deriving (Show)

type MeetingRes = Maybe ([Name], String)
data MeetingReq = MeetingReq {
    day1 :: Cal.Day
  , day2 :: Cal.Day
  , duration :: Clock.NominalDiffTime
  , employees :: [Name]
} deriving (Show)

type Scheduler = StateT Employees (Writer [MeetingReq])

runScheduler :: Scheduler a -> [MeetingReq]
runScheduler = execWriter . flip execStateT Data.Map.empty

----------------------
-- Core functionality
----------------------

-- change result type to [MeetingRes]
schedule :: [Scheduler a] -> [MeetingReq]
schedule entries = []
    where ((_,employees),reqs) = runWriter $ runStateT (sequence_ entries) (Data.Map.empty)

findMeetingTime :: Employees -> [MeetingReq] -> [MeetingRes]
findMeetingTime employees [] = []
findMeetingTime employees ((MeetingReq d1 d2 dur emps):reqs) = undefined --unionWith (++)  
    -- fold over the list of names and get their availabilities. 
    where avails = foldl (\acc x -> (fromJust $ flip Data.Map.lookup employees x) : acc) [] emps

combineAvailability :: Cal.Day -> Employees -> Name -> Name -> Availability
combineAvailability day emps p1 p2 = fromMaybe (singleton day []) (combine <$> p1Avail <*> p2Avail)
    where p1Avail = Data.Map.lookup p1 emps
          p2Avail = Data.Map.lookup p2 emps

-- combines two availabilities by finding overlap for each day
combine :: Availability -> Availability -> Availability
combine = intersectionWith combineDurs

-- combines two lists of durations found in the same day
combineDurs :: [Duration] -> [Duration] -> [Duration]
combineDurs [] _ = []
combineDurs _ [] = []
combineDurs (d:ds) ds' = concatMap (combineDuration d) ds' ++ combineDurs ds ds' 
    
-- combines two durations
combineDuration :: Duration -> Duration -> [Duration]
combineDuration (Duration t1 t2) (Duration t1' t2')
    | t2 < t1' = []
    | otherwise = [Duration combT1 combT2]
    where combT1 = max t1 t1'
          combT2 = min t2 t2'

-- combines additional availiabity to already existing availability
addAvailability :: Availability -> Availability -> Availability
addAvailability = unionWith (++)

-- adds availability of person to Availability in Employees map
available :: Name -> String -> Duration ->  Scheduler ()
available name dateStr dur = do
    env <- get
    put $ (insertWith addAvailability name $ singleton day [dur]) env 
    where day = parseDate dateStr
    
meeting :: [Name] -> Clock.NominalDiffTime -> String -> String -> Scheduler () 
meeting names timeLength d1 d2 = tell $ [MeetingReq d1' d2' timeLength names]
    where d1' = parseDate d1
          d2' = parseDate d2

--------------
-- Helpers
--------------

hours :: Integer -> Clock.NominalDiffTime
hours = realToFrac . Clock.secondsToDiffTime . (*3600) 

minutes :: Integer -> Clock.NominalDiffTime
minutes = realToFrac . Clock.secondsToDiffTime . (*60)
 
to :: String -> String -> Duration
t1 `to` t2 
    | t1' < t2' = Duration t1' t2'
    | otherwise = error "Availability start time must be less than end time"
    where t1' = parseTime t1
          t2' = parseTime t2

parseTime :: String -> Clock.NominalDiffTime 
parseTime t = 
    case map read (splitOn ":" t) of
        [h1,m1] -> let secs = h1*3600 + m1*60
                   in realToFrac $ Clock.secondsToDiffTime secs
        _       -> error . show $ TimeFormatErr (t ++ " arg to `to` must be in fmt HH:MM") 

parseDate :: String -> Cal.Day
parseDate date = 
    case map read $ splitOn "-" date of
        [yr,mn,dy] -> Cal.fromGregorian yr (fromIntegral mn) (fromIntegral dy)
        _          -> error . show $ DateFormatErr (date ++ " arg to `available` must be in fmt YYYY-MM-DD")
