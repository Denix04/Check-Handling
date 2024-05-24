module Main where

import Graphics.UI.Gtk
import SignalHandlers
import UI
    
main :: IO ()
main = do
    _ <- initGUI 
    window <- mainWindow
    mainCont <- mainContainer window

    menuBar mainCont
    headerRow mainCont
    tabla <- mainTable mainCont
    foot mainCont

    widgetShowAll window
    _ <- on window objectDestroy $ quitProgram tabla

    mainGUI
    
