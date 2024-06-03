module DataManipulation where

import Data.Char
import Data.Maybe
import Data

-----------------------------
-- Date
-----------------------------

strToDate :: String -> Either Int Date
strToDate s =
    maybe (Left 0) parseToDate (sequence $ map strToNumber $ separateBy sep s)
    
    where 
        sep = ['/',' ',',','.']
        parseToDate [d,m,y] = let date = Date d m y in 
            if validDate date then Right date else Left 0
        parseToDate _ = Left 0

-----------------------------
-- Type Operation
-----------------------------

typeOpToStr :: TypeOperation -> String
typeOpToStr Income = "Ingreso"
typeOpToStr Egress = "Egreso"
typeOpToStr ToEgress = "A Egresar"

guesTypeOp :: String -> Either Int TypeOperation
guesTypeOp [] = Left 2
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


        mostSimilar :: [(TypeOperation, Int)] -> Either Int TypeOperation
        mostSimilar xs = aux 0 (Left 2) xs
            where 
                aux _ ret [] = ret
                aux acc ret ((typ,cont):rest)
                    | cont > acc = aux cont (Right typ) rest
                    | otherwise = aux acc ret rest

-----------------------------
-- Amount
-----------------------------

strToCheckId :: String -> Either Int Int
strToCheckId s = maybe (Left 1) (\x -> Right x) $ strToNumber s

strToAmount :: String -> Either Int Double
strToAmount s = maybe (Left 3) (\x -> Right x) $ strToNumber s

-----------------------------
-- Registers
-----------------------------

listToRegister :: [String] -> Either Int Register
listToRegister [date',checkId',state',amount',desc] =
    strToDate date' >>= \d ->
    strToCheckId checkId' >>= \c ->
    guesTypeOp state' >>= \s ->
    strToAmount amount' >>= \a ->
    Right $ Register { 
        date = (d),
        state = (s),
        checkId = (c),
        amount = (a),
        description = desc }
listToRegister _ = Left 1

strToRegisters :: [[String]] -> Registers
strToRegisters table = catEithers $ map listToRegister table

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

strToNumber ::Read a => String -> Maybe a
strToNumber [] = Nothing
strToNumber s = 
    if validNumber s 
        then Just $ read s 
        else Nothing
    where
        validNumber s = foldr (&&) True $ map validChar s

        validChar c = isDigit c || c == '.'

catEithers :: [Either a b] -> [b]
catEithers (x:xs) =
    case x of
        Left _ -> catEithers xs
        Right e -> e:catEithers xs
    
