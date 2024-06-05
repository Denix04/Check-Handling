module Data where

data Date = Date {
    day :: Int,
    month :: Int,
    year :: Int }

data TypeOperation = Income | Egress | ToEgress | None
    deriving (Eq)

data Register = 
    Register {
        date :: Date,
        state :: TypeOperation,
        checkId :: Int,
        amount :: Double,
        description :: String }

type Registers = [Register]

instance Show Register where
    show (Register d s c a des) = 
        "Registro\n" ++
        "\t" ++ show d ++ "\n" ++
        "\t" ++ show s ++ "\n" ++
        "\t" ++ show c ++ "\n" ++
        "\t" ++ show a ++ "\n" ++
        "\t" ++ show des ++ "\n"

instance Show Date where
    show d = (show $ day d) ++ "/" ++ 
             (show $ month d) ++ "/" ++ 
             (show $ year d)

instance Show TypeOperation where
    show Income = "Ingreso"
    show Egress = "Egreso"
    show ToEgress = "A Egresar"
    show None = ""

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
