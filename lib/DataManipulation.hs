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
-- Type 1
-----------------------------

strToOpType :: String -> Either Int OpType
strToOpType s = 
    case guessConstructor s [Income,Egress,ToEgress] of
        Nothing -> Left 1
        Just opType -> Right opType

-----------------------------
-- Method 2
-----------------------------

strToOpMethod :: String -> Either Int OpMethod
strToOpMethod s = 
    case guessConstructor s [Transfer,Check,Cash,BankExpense] of
        Nothing -> Left 2
        Just opMethod -> Right opMethod

-----------------------------
-- Id 3
-----------------------------

strToOpId :: String -> Either Int Id
strToOpId [] = Left 3
strToOpId s =
    maybe (Right $ Person s) (\x -> Right $ Num x) $ strToNumber s

-----------------------------
-- Amount 4
-----------------------------

strToAmount :: String -> Either Int Double
strToAmount s = maybe (Left 4) (\x -> Right x) $ strToNumber s

-----------------------------
-- Registers
-----------------------------

listToRegister :: [String] -> Either Int Register
listToRegister [date',opType',opMethod',opId',opAmt',desc] =
    strToDate date' >>= \d ->
    strToOpType opType' >>= \t ->
    strToOpMethod opMethod' >>= \m ->
    strToOpId opId' >>= \i ->
    strToAmount opAmt' >>= \a ->
    Right $ Register { 
        date = (d),
        opType = (t),
        opMethod = (m),
        opId = (i),
        opAmt = (a),
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
strToNumber s = 
    case reads s of
        [(n,"")] -> Just n
        _ -> Nothing

catEithers :: [Either a b] -> [b]
catEithers (x:xs) =
    case x of
        Left _ -> catEithers xs
        Right e -> e:catEithers xs

guessConstructor :: Show a => String -> [a] -> Maybe a
guessConstructor s xs = (mostSimilar . (coincidences xs) . toLowerCase) s
    where
        toLowerCase = map toLower

        coincidences :: Show a => [a] -> String -> [(a, Int)]
        coincidences xs s = zip xs $ map ((count 0 s) . toLowerCase . show) xs
            where 
                count acc _ [] = acc
                count acc [] _ = acc
                count acc (s:ss) (x:xs) 
                    | s == x = count (acc+1) ss xs
                    | otherwise = count (acc) ss xs


        mostSimilar :: [(a, Int)] -> Maybe a
        mostSimilar xs = aux 0 Nothing xs
            where 
                aux _ return [] = return
                aux acc return ((typ,cont):rest)
                    | cont > acc = aux cont (Just typ) rest
                    | otherwise = aux acc return rest
guessConstructor _ _ = Nothing

