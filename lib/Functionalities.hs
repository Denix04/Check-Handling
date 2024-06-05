module Functionalities where

import Data.IORef
import Data

calculateTotal :: IORef Double -> IORef Registers -> IO ()
calculateTotal total registers = do
    regs <- readIORef registers
    writeIORef total $ calculate regs

    where
        calculate [] = 0
        calculate (x:xs)
            | opType x == Income = opAmt x + calculate xs
            | opType x == Egress = (- opAmt x) + calculate xs
            | opType x == ToEgress = calculate xs
