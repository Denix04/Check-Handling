module Main where

import GtkExtension
import Graphics.UI.Gtk
    
main :: IO ()
main = do
    _ <- initGUI 
    window <- windowNew
    windowSetDefaultSize window 600 600
    --windowMaximize window

    window' <- vBoxNew False 5
    containerAdd window window'

    -- Opciones--------

    opciones <- hBoxNew True 3
    boxPackStart window' opciones PackNatural 0

    file <- newMenuOption "Files" ["Cargar","Guardar","Nuevo"]
    opVista <- newMenuOption "Vista" ["Modo","Text","Full Screen"]
    opHelp <- newMenuOption "Help" ["Atajos","Licencia","Sobre"]

    boxPackStartGrow opciones [file,opVista,opHelp] 0


    -- Campos--------

    campos <- hBoxNew True 0
    boxPackStart window' campos PackNatural 0

    fechaBut <- buttonNewWithLabel "Fecha"
    numBut <- buttonNewWithLabel "n°"
    tipoBut <- buttonNewWithLabel "Tipo"
    montoBut <- buttonNewWithLabel "Monto"
    descBut <- buttonNewWithLabel "Descripcion"

    let buttons = [fechaBut,numBut,tipoBut,montoBut,descBut]
    boxPackStartGrow campos buttons 0


    -- Tabla --------

    tabla <- newTable
    boxPackStart window' tabla PackGrow 0

    appendCell tabla
    appendCell tabla
   

    -- Pie de pagina ----

    pie <- hBoxNew False 0
    boxPackEnd window' pie PackNatural 10

    eti1 <- labelNewWithMnemonic "Hora"
    eti2 <- labelNewWithMnemonic "nose"
    eti3 <- labelNewWithMnemonic "algo"
    eti4 <- labelNewWithMnemonic "Total"

    boxPackStartGrow pie [eti1,eti2,eti3,eti4] 0


    widgetShowAll window
    _ <- on window objectDestroy $ salir tabla

    mainGUI
