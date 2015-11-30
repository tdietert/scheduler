module Main where
 
import Scheduler (schedule, available, meeting, hours, minutes, to)

main = print $ schedule $
    [ available "Hans"   "2015-10-26" ("10:00" `to` "12:00")
    , available "Hans"   "2015-10-26" ("13:00" `to` "17:00")
    , available "Hans"   "2015-10-28" ("13:00" `to` "17:00")
    , available "Philip" "2015-10-26" ("09:30" `to` "17:00")
    , available "Philip" "2015-10-28" ("09:30" `to` "12:00")
    , available "Tom"    "2015-10-26" ("09:30" `to` "16:00")
    , available "Tom"    "2015-10-27" ("09:30" `to` "16:00")
    , meeting   ["Philip", "Hans", "Tom"] (hours 1 + minutes 15) "2015-10-26" "2015-10-30"
    , meeting   ["Hans", "Tom"]           (hours 1)              "2015-10-26" "2015-10-30"
    ]

