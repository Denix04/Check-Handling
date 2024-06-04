module CSV where

import System.IO
import System.IO.Error
import Data.Maybe
import Control.Exception
import Data.IORef
import Data

load :: String -> IO (Maybe [[String]])
load path = do
    dataCsv <- try (readFile path) :: IO (Either IOError String)
    case dataCsv of 
        Left ex 
            | isDoesNotExistError ex ->
                getLine >>= (\s -> writeFile path s) >>
                putStrLn "se ha creado uno con exito."
                >> return (Just [])
            | otherwise -> putStrLn ("Falla en cargar el archivo " ++ show ex) >>
                   return Nothing
        Right [] -> return (Just [])
        Right d -> return (Just (map (split ',') (lines d)))

load' :: String -> IO (Maybe [[String]])
load' path = do
    dataCsv <- try (readFile path) :: IO (Either IOError String)
    either notRead read dataCsv

    where
        notRead ex
            | isDoesNotExistError ex =
                writeFile path "Fecha,N°,Tipo,Monto,Descripcion" >>
                return (Just [])
            | otherwise =
                putStrLn ("Wasn't posible load the file" ++ show ex) >>
                return Nothing

        read [] = return (Just [])
        read d = return (Just (map (split ',') (lines d)))

toRegisterCsv :: Register -> String
toRegisterCsv (Register (Date d m y) state checkId amount desc) =
    show d ++ "," ++ 
    show m ++ "," ++ 
    show y ++ "," ++ 
    show state ++ "," ++ 
    show checkId ++ "," ++ 
    show amount ++ 
    desc

toCsv :: Registers -> String
toCsv regs = unlines (map toRegisterCsv regs)


save :: String -> IORef Registers -> IO ()
save path regs = do
    info <- show <$> readIORef regs
    dataCsv <- try (writeFile path info) :: IO (Either IOError ())
    case dataCsv of
        Left ex -> putStrLn $ "Fail to save the file" ++ show ex
        Right _ -> putStrLn "File saved succesfully"

-----------------------------
-- Utilities
-----------------------------

split :: Char -> String -> [String]
split _ [] = []
split c s = [takeWhile (\x -> x /= c) s] ++ 
           split c (drop 1 (dropWhile (\x -> x /= c) s))
