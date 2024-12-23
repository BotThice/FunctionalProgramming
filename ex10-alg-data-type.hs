data Month = January | February | March | April | May | June 
    | July | August | September | October | November | December
    deriving (Show)

daysInMonth :: Month -> Integer
daysInMonth month = case month of
    January -> 31
    February -> 28
    March -> 31
    April -> 30
    May -> 31
    June -> 30
    July -> 31
    August -> 31
    September -> 30
    October -> 31
    November -> 30
    December -> 31

nextMonth :: Month -> Month
nextMonth month = case month of
    January -> February
    February -> March
    March -> April
    April -> May
    May -> June
    June -> July
    July -> August
    August -> September
    September -> October
    October -> November
    November -> December
    December -> January

nextDay :: Integer -> Month -> (Integer, Month)
nextDay day month
    | day <= 0 = error "Invalid day"
    | day < daysInMonth month = (day + 1, month)
    | otherwise = (1, nextMonth month)