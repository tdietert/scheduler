module Scheduler (
    schedule
  , available
  , meeting
  , hours
  , minutes
  , to
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
import qualified Data.Map as Map
import Data.Maybe

data InputError = DateFormatErr String
                | TimeFormatErr String
    deriving (Show)

type Name = String
type Availability = Map.Map Cal.Day [Duration]
type Employees = Map.Map Name Availability
data Duration = Duration Clock.NominalDiffTime Clock.NominalDiffTime
    deriving (Show, Eq)

data MeetingTime = MeetingTime Cal.Day Duration
instance Show MeetingTime where 
    show (MeetingTime day (Duration s e)) =
        show day ++ " " ++ diffTimeToHHMM s ++ "-" ++ diffTimeToHHMM e

type MeetingRes = ([Name], MeetingTime)
data MeetingReq = MeetingReq {
    day1 :: Cal.Day
  , day2 :: Cal.Day
  , duration :: Clock.NominalDiffTime
  , employees :: [Name]
} deriving (Show)

type Scheduler = StateT Employees (Writer [MeetingReq])

runScheduler :: Scheduler a -> [MeetingReq]
runScheduler = execWriter . flip execStateT Map.empty

----------------------
-- Core functionality
----------------------

schedule :: [Scheduler a] -> [MeetingRes]
schedule entries = makeMeeting employees reqs []
    where ((_,employees),reqs) = runWriter $ runStateT (sequence_ entries) Map.empty

makeMeeting :: Employees -> [MeetingReq] -> [MeetingRes] -> [MeetingRes] 
makeMeeting employees [] res = res 
makeMeeting employees ((MeetingReq d1 d2 timelen emps):reqs) res =
    case findMeetingTimes combinedAvails timelen d1 of
        Just durs -> 
           let possibleMeets = concatMap (\dur -> [(emps,MeetingTime d1 dur),(emps,MeetingTime d2 dur)]) durs in
           case findPossibleMeetings possibleMeets res of
               Just meeting -> makeMeeting employees reqs (meeting:res)
               Nothing -> makeMeeting employees reqs res
    -- fold over the list of names and get their availabilities. 
    where avails = foldl (getAvailabilities employees) [] emps
          combinedAvails = foldl combineAvailability Map.empty avails

getAvailabilities :: Employees -> [Availability] -> Name -> [Availability]
getAvailabilities employees avails name = 
    case Map.lookup name employees of
        Just a -> a : avails
        Nothing -> error $ "Employee " ++ name ++ " not found!"

findMeetingTimes :: Availability -> Clock.NominalDiffTime -> Cal.Day -> Maybe [Duration]
findMeetingTimes avail timelen day =
    case Map.lookup day avail of
        Just durations -> 
            case durations of
                (d:durs) -> sequence $ map (withinDuration timelen) (d:durs)
                [] -> Nothing
        Nothing -> Nothing         
        
withinDuration :: Clock.NominalDiffTime -> Duration -> Maybe Duration
withinDuration timelen (Duration s e) 
    | e - s >= timelen = Just $ Duration s (s + timelen)
    | otherwise = Nothing

findPossibleMeetings :: [MeetingRes] -> [MeetingRes] -> Maybe MeetingRes
findPossibleMeetings [] _ = Nothing
findPossibleMeetings (m:ms) [] = Just m
findPossibleMeetings (m:ms) ms' = if checkMeetingTime m ms' then Just m
                                  else findPossibleMeetings ms ms'

-- search through existing meeting suggestions for overlap
checkMeetingTime :: MeetingRes -> [MeetingRes] -> Bool
checkMeetingTime (names,time) meetings = 
    foldl (\acc (names', time') ->
              if or (map (`elem` names') names)
              then timesOverlap time time' || acc
              else False || acc            
          ) False meetings 

timesOverlap :: MeetingTime -> MeetingTime -> Bool
timesOverlap (MeetingTime day1 dur1) (MeetingTime day2 dur2)
    | day1 == day2 = if (combineDuration dur1 dur2) == [] then True else False
    | otherwise = False                      

-- combines two availabilities by finding overlap for each day
combineAvailability :: Availability -> Availability -> Availability
combineAvailability = Map.unionWith combineDurations

-- combines two lists of durations found in the same day
combineDurations :: [Duration] -> [Duration] -> [Duration]
combineDurations [] _ = []
combineDurations _ [] = []
combineDurations (d:ds) ds' = concatMap (combineDuration d) ds' ++ combineDurations ds ds' 
    
-- combines two durations
combineDuration :: Duration -> Duration -> [Duration]
combineDuration (Duration t1 t2) (Duration t1' t2')
    | t2 < t1' || t2' < t1 = []
    | otherwise = [Duration combT1 combT2]
    where combT1 = max t1 t1'
          combT2 = min t2 t2'

-- combines additional availiabity to already existing availability
addAvailability :: Availability -> Availability -> Availability
addAvailability = Map.unionWith (++)

-- adds availability of person to Availability in Employees map
available :: Name -> String -> Duration ->  Scheduler ()
available name dateStr dur = do
    env <- get
    put $ (Map.insertWith addAvailability name $ Map.singleton day [dur]) env 
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

diffTimeToHHMM :: Clock.NominalDiffTime -> String
diffTimeToHHMM t = fst $ splitAt (length todStr - 3) todStr
    where todStr = show . LocT.timeToTimeOfDay $ realToFrac t
  
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
