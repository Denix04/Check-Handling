module Functionalities where

import Data.IORef
import Data

calculateTotal :: Programm -> IO ()
calculateTotal prog = do
    regs <- readIORef $ progRegisters prog
    writeIORef (progTotalAmt prog) $ calculate regs

    where
        calculate [] = 0
        calculate (x:xs)
            | opType x == Income = opAmt x + calculate xs
            | opType x == Egress = (- opAmt x) + calculate xs
            | opType x == ToEgress = calculate xs

addToTotal :: Register -> Programm -> IO ()
addToTotal reg prog =
    readIORef (progTotalAmt prog) >>= \x ->
    writeIORef (progTotalAmt prog) (x + opAmt reg)
