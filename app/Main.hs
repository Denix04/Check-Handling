module Main where

import Data.IORef
import Graphics.UI.Gtk
import Data
import DataRetrieve
import DataManipulation
import SignalHandlers
import GtkExtension
import UI

loadProgramm :: (IORef Double,IORef Registers)
loadProgramm = undefined
    

main :: IO ()
main = do
    _ <- initGUI 
    window <- mainWindow
    mainCont <- mainContainer window

    programm <- newProgramm

    menuBar mainCont
    headerRow mainCont
    tabla <- mainTable mainCont programm
    foot mainCont programm

    widgetShowAll window
    _ <- on window objectDestroy $ quitProgram "prueba.csv" programm --cells

    mainGUI
