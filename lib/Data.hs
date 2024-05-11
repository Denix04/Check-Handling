module Data where

import Data.Time.Clock
import Data.ByteString.Lazy

data Tipo = Entrada | Salida

data Registro = 
    Registro {
        date :: UTCTime,
        num :: Double,
        tipo :: Tipo,
        monto :: Double,
        description :: ByteString }
