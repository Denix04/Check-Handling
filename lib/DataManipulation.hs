module DataManipulation where

import Data.Char
import Data.Maybe
import Data

-----------------------------
-- Date
-----------------------------

strToDate :: String -> Maybe Date
strToDate s = (sequence $ map strToNumber $ separateBy sep s) >>=
    parseToDate
    
    where 
        sep = ['/',' ',',','.']
        parseToDate [d,m,y] = let date = Date d m y in 
            if validDate date then Just date else Nothing
        parseToDate _ = Nothing

-----------------------------
-- Type Operation
-----------------------------

typeOpToStr :: TypeOperation -> String
typeOpToStr Income = "Ingreso"
typeOpToStr Egress = "Egreso"
typeOpToStr ToEgress = "A Egresar"

guesTypeOp :: String -> Maybe TypeOperation
guesTypeOp [] = Nothing
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


        mostSimilar :: [(TypeOperation, Int)] -> Maybe TypeOperation
        mostSimilar xs = aux 0 Nothing xs
            where 
                aux _ ret [] = ret
                aux acc ret ((typ,cont):rest)
                    | cont > acc = aux cont (Just typ) rest
                    | otherwise = aux acc ret rest

-----------------------------
-- Amount
-----------------------------

strToNumber ::Read a => String -> Maybe a
strToNumber [] = Nothing
strToNumber s = 
    if validNumber s 
        then Just $ read s 
        else Nothing
    where
        validNumber [] = True
        validNumber (x:xs) = validChar x && validNumber xs

        validChar c = isDigit c || c == '.'

-----------------------------
-- Registers
-----------------------------

listToRegister :: [String] -> Maybe Register
listToRegister [date',checkId',state',amount',desc] =
    strToDate date' >>= \d ->
    strToNumber checkId' >>= \c ->
    strToNumber amount' >>= \a ->
    guesTypeOp state' >>= \s ->
    Just $ Register { 
        date = (d),
        state = (s),
        checkId = (c),
        amount = (a),
        description = desc }
listToRegister _ = Nothing

strToRegisters :: [[String]] -> Registers
strToRegisters table = catMaybes $ map listToRegister table

-----------------------------
-- Utilities
-----------------------------
    
separateBy :: String -> [Char] -> [String]
separateBy s sep = aux s sep [] []
    where
        aux :: [Char] -> String -> String -> [String] -> [String]
        aux sep [] acc ss = ss ++ [acc]
        aux sep (x:xs) acc ss
            | x `elem` sep &&  acc == [] = aux sep xs [] ss
            | x `elem` sep = aux sep xs [] (ss ++ [acc])
            | otherwise  = aux sep xs (acc ++ [x]) ss
