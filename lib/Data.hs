module Data where

data Date = Date { day :: Int, month :: Int, year :: Int }

data OpType = Income | Egress | ToEgress
    deriving (Eq)

data OpMethod = Transfer | Check | Cash | BankExpense
    deriving (Eq)

data Id = Num Int | Person String

data Register = 
    Register {
        date :: Date,
        opType :: OpType,
        opMethod :: OpMethod,
        opId :: Id,
        opAmt :: Double,
        description :: String }

type Registers = [Register]

instance Show Register where
    show (Register date ot om ci oa des) = 
        show date ++ "," ++
        show ot ++ "," ++
        show om ++ "," ++
        show ci ++ "," ++
        show oa ++ "," ++
        show des ++ "\n"

instance Show Date where
    show (Date d m y) =
        show d ++ "," ++
        show m ++ "," ++
        show y

instance Show OpType where
    show Income = "Ingreso"
    show Egress = "Egreso"
    show ToEgress = "A Egresar"

instance Show OpMethod where
    show Transfer = "Transferencia"
    show Check = "Cheque"
    show Cash = "Efectivo"
    show BankExpense = "Banco"

instance Show Id where
    show (Num id) = show id
    show (Person id) = id

validDate :: Date -> Bool
validDate (Date d m y)
    | m `elem` [1,3,5,8,10,12] = valid31 d
    | m `elem` [4,6,7,9,11] = valid30 d
    | m `elem` [2] = validFeb y d
    | otherwise = False
        where 
            valid31 d
                | d < 1 || d > 31 = False
                | otherwise = True
            
            valid30 d
                | d < 1 || d > 30 = False
                | otherwise = True

            validFeb y d
                | mod y 4 == 0 
                  && (mod y 100 /= 0 || mod y 400 == 0) 
                  && d <= 29 = True
                | d < 1 || d > 28 = False
                | otherwise = True
