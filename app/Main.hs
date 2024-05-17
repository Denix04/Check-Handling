module Main where

import GtkExtension
import ShortCuts
import Graphics.UI.Gtk
    
main :: IO ()
main = do
    _ <- initGUI 
    window <- windowNew
    windowMaximize window

    window' <- vBoxNew False 5
    containerAdd window window'

    -- Opciones--------

    opciones <- hBoxNew True 3
    boxPackStart window' opciones PackNatural 0

    file <- newMenuOption "Files" ["Cargar","Guardar","Nuevo"]
    opVista <- newMenuOption "Vista" ["Modo","Text","Full Screen"]
    opHelp <- newMenuOption "Help" ["Atajos","Licencia","Sobre"]

    boxPackStart opciones file PackGrow 0
    boxPackStart opciones opVista PackGrow 0
    boxPackStart opciones opHelp PackGrow 0


    -- Campos--------

    campos <- hBoxNew True 0
    boxPackStart window' campos PackNatural 0

    fechaBut <- buttonNewWithLabel "Fecha"
    numBut <- buttonNewWithLabel "n°"
    tipoBut <- buttonNewWithLabel "Tipo"
    montoBut <- buttonNewWithLabel "Monto"
    descriptionBut <- buttonNewWithLabel "Descripcion"

    boxPackStart campos fechaBut PackGrow 0
    boxPackStart campos numBut PackGrow 0
    boxPackStart campos tipoBut PackGrow 0
    boxPackStart campos montoBut PackGrow 0
    boxPackStart campos descriptionBut PackGrow 0


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

    boxPackStart pie eti1 PackGrow 0
    boxPackStart pie eti2 PackGrow 0
    boxPackStart pie eti3 PackGrow 0
    boxPackStart pie eti4 PackGrow 0


    --_ <- shortCutsManage window fechaBut
    --_ <- shortCutsManage window

    widgetShowAll window
    _ <- on window objectDestroy $ salir tabla

    mainGUI
