import System.Random (randomRIO)
import Control.Applicative ((<$>))
import System.Exit
import Data.Char

type Day = Int 
type Year = Int
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Read,Show,Enum,Eq,Ord)
months = [January .. December]
next = toEnum . (`mod` (length months)) . (+1) . fromEnum

type Date = (Month, Day, Year)

-- first base year in the 1900's
startYear = 1905 
endYear = 2100
-- due to shortcut in leapYearsSince calculation this will only be accurate between the years [1905-2100)

dayOf :: Date -> Int
dayOf (month, day, year) = mod (day + monthValue month + yearValue year + leapCorrection (month,year)) 7

--determined by which day of the week May 7th 1905 is
monthValue :: Month -> Int
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

yearValue :: Year -> Int
yearValue year = year - startYear + leapYearsSince startYear year

leapYearsSince :: Year -> Year -> Int
leapYearsSince x y = ((y - x) + x`mod`4) `div` 4

isLeapYear :: Year -> Bool
isLeapYear year = leapYearsSince (year - 1) year == 1

leapCorrection :: (Month,Year) -> Int
leapCorrection (month, year) = if (month <= February && isLeapYear year) then -1 else 0

--

selectDate :: Int -> Date
selectDate n = let (month,year) = (selectMonth n, selectYear n) in (month, selectDay (month,year) n, year)

selectMonth :: Int -> Month
selectMonth i = months !! (i `mod` length months)

isLastMonthDay :: Date -> Bool
isLastMonthDay (month,day,year) = dayOf (month, day + 1, year) == dayOf (next month, 1, year)

shortedMonthLength = 28

lastMonthDay :: (Month,Year) -> Day
lastMonthDay (month,year) = (\(_,day,_) -> day) . head . filter isLastMonthDay . map (\day -> (month,day,year)) $ [shortedMonthLength..]

selectYear :: Int -> Year
selectYear = (startYear+) . (`mod`(endYear - startYear))

selectDay :: (Month,Year) -> Int -> Day
selectDay (month,year) = (+1) . (`mod` lastMonthDay (month,year))

readDays :: String -> [Int]
readDays = map read . filter (all isNumber) . lines

showDate :: Date -> String
showDate = tail . map (\c -> if isAlphaNum c then c else ' ') . show 

main = do
    date <- selectDate <$> randomRIO (minBound,maxBound)
    putStrLn . showDate $ date
    numFailures <- length . takeWhile (/= dayOf date) . readDays <$> getContents
    if numFailures == 0
        then exitSuccess
        else exitWith (ExitFailure numFailures)
