module DataManipulation where

import Data.Char
import Data

-----------------------------
-- Date
-----------------------------

strToDate :: String -> Maybe Date
strToDate s = do
    case separateBy sep s of 
        [day',month',year'] ->
            let date = Date {day = read day', 
                            month = read month', 
                            year = read year'}
            in 
                if validDate date 
                    then Just date 
                    else Nothing

        _ -> Nothing
    
    where 
        sep = ['/',' ',',','.']

dateToStr :: Date -> String
dateToStr (Date d m y) = show d ++ "/" ++
                         show m ++ "/" ++
                         show y

-----------------------------
-- Type Operation
-----------------------------

typeOpToStr :: TypeOperation -> String
typeOpToStr Income = "Ingreso"
typeOpToStr Egress = "Egreso"
typeOpToStr ToEgress = "A Egresar"

strToTypeOp :: String -> TypeOperation
strToTypeOp "Ingreso" = Income
strToTypeOp "Egreso" = Egress
strToTypeOp "A Egresar" = ToEgress


guesTypeOp :: String -> TypeOperation
guesTypeOp s = (mostSimilar . coincidences . toLowerCase) s
    where
        toLowerCase = map toLower

        coincidences :: String -> [(TypeOperation, Int)]
        coincidences s = zip typeOp $ map (aux 0 s) typeOpStr
            where 
                aux acc _ [] = acc
                aux acc [] _ = acc
                aux acc (s:ss) (x:xs) 
                    | s == x = aux (acc+1) ss xs
                    | otherwise = aux (acc) ss xs

                typeOp = [Income,Egress,ToEgress]
                typeOpStr = ["ingreso","egreso","a egresar"]


        mostSimilar :: [(TypeOperation, Int)] -> TypeOperation
        mostSimilar xs = aux 0 None xs
            where 
                aux _ to [] = to
                aux acc to ((t,c):xs)
                    | c > acc = aux c t xs
                    | otherwise = aux acc to xs

-----------------------------
-- Registers
-----------------------------

listToRegister :: [String] -> Maybe Register
listToRegister [date',state',checkId',amount',desc] =
    Just Register { 
        --date = (strToDate date'),
        state = (strToTypeOp state'),
        checkId = (read checkId'),
        amount = (read amount'),
        description = desc }

getRegisters :: IO [Register]
getRegisters = undefined
    --getDataTable >>= 
    
separateBy :: String -> [Char] -> [String]
separateBy s sep = aux s sep [] []
    where
        aux :: [Char] -> String -> String -> [String] -> [String]
        aux sep [] acc ss = ss ++ [acc]
        aux sep (x:xs) acc ss
            | x `elem` sep && acc == [] = aux sep xs [] ss
            | x `elem` sep = aux sep xs [] (ss ++ [acc])
            | otherwise  = aux sep xs (acc ++ [x]) ss
