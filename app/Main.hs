module Main where

import GtkExtension
import ShortCuts
import Graphics.UI.Gtk
    
main :: IO ()
main = do
    _ <- initGUI 
    window <- windowNew
    windowSetDefaultSize window 600 600

    window' <- vBoxNew False 0
    containerAdd window window'

    -- Opciones--------

    opciones <- hBoxNew True 2

    file <- newMenuOption "Files" ["Cargar","Guardar","Nuevo"]
    opVista <- newMenuOption "Vista" ["Modo","Text","Full Screen"]
    opHelp <- newMenuOption "Help" ["Atajos","Licencia","Sobre"]

    containerAdd opciones file
    containerAdd opciones opVista
    containerAdd opciones opHelp

    containerAdd window' opciones

    -- Campos--------

    campos <- hBoxNew True 2

    fechaBut <- buttonNewWithLabel "Fecha"
    numBut <- buttonNewWithLabel "n°"
    tipoBut <- buttonNewWithLabel "Tipo"
    montoBut <- buttonNewWithLabel "Monto"
    descriptionBut <- buttonNewWithLabel "Descripcion"

    containerAdd campos fechaBut
    containerAdd campos numBut
    containerAdd campos tipoBut
    containerAdd campos montoBut
    containerAdd campos descriptionBut

    containerAdd window' campos

    -- Tabla --------

    tabla <- tableNew 2 1 True

    appendCell tabla 0
    appendCell tabla 1

    containerAdd window' tabla

    ----------------------
    --_ <- shortCutsManage window fechaBut
    _ <- shortCutsManage window tabla

    widgetShowAll window
    _ <- on window objectDestroy mainQuit

    mainGUI
