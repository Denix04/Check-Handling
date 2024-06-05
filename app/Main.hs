module Main where

import Data.IORef
import Graphics.UI.Gtk
import Data
import DataRetrieve
import DataManipulation
import SignalHandlers
import UI

loadProgramm :: (IORef Double,IORef Registers)
loadProgramm = undefined
    
main :: IO ()
main = do
    _ <- initGUI 
    window <- mainWindow
    mainCont <- mainContainer window

    registers <- newIORef [] :: IO (IORef Registers)

    menuBar mainCont
    headerRow mainCont
    tabla <- mainTable mainCont registers
    foot mainCont

    widgetShowAll window
    _ <- on window objectDestroy $ quitProgram "prueba.csv" registers

    mainGUI
