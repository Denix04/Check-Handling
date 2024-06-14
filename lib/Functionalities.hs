module Functionalities where

import Data.IORef
import Data

calculateTotal :: Programm -> IO ()
calculateTotal prog = do
    regs <- readIORef $ progRegisters prog
    writeIORef (progTotalAmt prog) $ calculate 0 regs

addToTotal :: Programm -> Register -> IO ()
addToTotal prog reg =
    readIORef (progTotalAmt prog) >>= \total ->
    writeIORef (progTotalAmt prog) (calculate total [reg])

calculate :: Double -> Registers -> Double
calculate acc [] = acc
calculate acc (x:xs)
    | opType x == Income = seq (acc + opAmt x) (calculate (acc + opAmt x) xs)
    | opType x == Egress = seq (acc - opAmt x) (calculate (acc - opAmt x) xs)
    | opType x == ToEgress = calculate acc xs
