module DataManipulation where

import Data.Maybe
import Data
import Utilities

strToDate :: String -> Either Int Date
strToDate s =
    maybe (Left 0) parseToDate (sequence $ map strToNumber $ separateBy sep s)
    
    where 
        sep = ['/',' ',',','.']
        parseToDate [d,m,y] = let date = Date d m y in 
            if validDate date then Right date else Left 0
        parseToDate _ = Left 0

strToOpType :: String -> Either Int OpType
strToOpType s = 
    case guessConstructor s [Income,Egress,ToEgress] of
        Nothing -> Left 1
        Just opType -> Right opType

strToOpMethod :: String -> Either Int OpMethod
strToOpMethod s = 
    case guessConstructor s [Transfer,Check,Cash,BankExpense] of
        Nothing -> Left 2
        Just opMethod -> Right opMethod

strToOpId :: String -> Either Int Id
strToOpId [] = Left 3
strToOpId s =
    maybe (Right (Person s)) (\x -> Right (Num x)) $ strToNumber s

strToAmount :: String -> Either Int Double
strToAmount s = maybe (Left 4) (\x -> Right x) $ strToNumber s

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
