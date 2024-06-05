module Functionalities where

import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data

calculateTotal :: IORef Double -> IORef Registers -> IO ()
calculateTotal total registers = do
    regs <- readIORef registers
    writeIORef total $ calculate regs

    where
        calculate [] = 0
        calculate (x:xs)
            | state x == Income = amount x + calculate xs
            | state x == Egress = (- amount x) + calculate xs
            | state x == ToEgress = calculate xs
