module DataManipulation where

import Data

strToDate :: String -> Maybe Date
strToDate s = do
    case separateBy sep s of 
        [day',month',year'] ->
            let date = Date {day = read day', 
                            month = read month', 
                            year = read year'}
            in if validDate date then Just date else Nothing

        _ -> Nothing
    
    where 
        sep = ['/',' ',',','.']

strToTypeOp :: String -> TypeOperation
strToTypeOp "Ingreso" = Income
strToTypeOp "Egreso" = Egress
strToTypeOp "A Egresar" = ToEgress

listToRegister :: [String] -> Register
listToRegister [date',state',checkId',amount',desc] =
    Register { 
        date = (strToDate date'),
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
            | x `elem` sep = aux sep xs [] (ss ++ [acc])
            | otherwise  = aux sep xs (acc ++ [x]) ss
