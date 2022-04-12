module Main where

data Accommodation
    = AccHouse
    {
        acc_house_num :: Int,
        acc_street :: String,
        acc_floor_count :: Int
    }
   | AccFlat
    {
        acc_house_num :: Int,
        acc_street :: String,
        acc_floor :: Int
    }

isHouse (AccHouse _ _ _) = True
isHouse _ = False -- all other values are not a House

-- example accommodation:
myHouse1 = AccHouse 111 "Golden Avenue" 2
myFlat1 = AccFlat 81 "Silver Street" 0

accommodation1 = [myHouse1, myFlat1]

-- (accomOK accom) returns a boolean indicating whether
-- the accommodation record makes sense:
accomOK :: Accommodation -> Bool
accomOK (AccHouse num _ floors) =
    (floors > 0) && (num > 0)
accomOK (AccFlat num _ floor) =
    (floor >= 0) && (num > 0)

numbers1 = [ acc_house_num accom | accom <- accommodation1]

houses = [ accom | accom <- accommodation1, isHouse accom ]

main =
    do
    putStrLn $ "house numbers 1: " ++ (show numbers1)