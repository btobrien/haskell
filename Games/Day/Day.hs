import System.Random
import System.Exit
import Data.Char

also f x = (x, f x)

type Day = Int 
type Year = Int
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Read,Show,Enum,Eq,Ord)
type Date = (Month, Day, Year)

-- first base year in the 1900's
startYear = 1905 
endYear = 2100
-- due to rudimentary leapYearsSince calculation this will only be accurate between the years 1905 - 2099

dayOf :: Date -> Int
dayOf (month, day, year) = mod (day + monthValue month + yearValue year + leapCorrection (month,year)) 7

--determined by which day of the week May 7th 1905 is
monthValue January = 6
monthValue February = 2
monthValue March = 2
monthValue April = 5
monthValue May = 0
monthValue June = 3
monthValue July = 5
monthValue August = 1
monthValue September = 4
monthValue October = 6
monthValue November = 2
monthValue December = 4

yearValue year = year - startYear + leapYearsSince startYear year

leapCorrection :: (Month,Year) -> Int
leapCorrection (month, year) | month <= February && isLeapYear year = -1
leapCorrection _ | otherwise = 0

leapYearsSince :: Year -> Year -> Int
leapYearsSince x y = ((y - x) + x`mod`4) `div` 4

isLeapYear :: Year -> Bool
isLeapYear year = leapYearsSince (year - 1) year == 1

isBaseYear :: Year -> Bool
isBaseYear = (==0) . (`mod`7) . yearValue

--

selectDate :: Int -> Date
selectDate n = (selectMonth n, selectDay n, selectYear n)

selectDay :: Int -> Day
selectDay = (`mod`30)

selectMonth :: Int -> Month
selectMonth = toEnum . (`mod` (fromEnum December + 1))

selectYear :: Int -> Year
selectYear = (startYear+) . (`mod`(endYear - startYear))

readDays :: String -> [Int]
readDays = map read . lines

main = do
    date <- fmap selectDate (randomRIO (minBound,maxBound))
    print date
    numFailures <- fmap (length . (takeWhile (/= dayOf date)) . readDays) getContents
    if numFailures == 0
        then exitSuccess
        else exitWith (ExitFailure numFailures)
