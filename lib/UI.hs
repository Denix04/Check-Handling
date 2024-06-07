module UI where

import Graphics.UI.Gtk
import Data.IORef
import GtkExtension
import Data
import Utilities

mainWindow :: IO Window
mainWindow = do
    window <- windowNew
    windowSetDefaultSize window 1200 300

    return window

mainContainer :: Window -> IO VBox
mainContainer window = do
    mainCont <- vBoxNew False 5
    containerAdd window mainCont

    return mainCont

menuBar :: VBox -> IO ()
menuBar mainCont = do
    menuBar <- hBoxNew True 3
    boxPackStart mainCont menuBar PackNatural 0

    file <- newMenuOption "Files" ["Cargar","Guardar","Nuevo"]
    opVista <- newMenuOption "Vista" ["Modo","Text","Full Screen"]
    opHelp <- newMenuOption "Help" ["Atajos","Licencia","Sobre"]

    boxPackStartGrow menuBar [file,opVista,opHelp] 0

headerRow :: VBox -> IO ()
headerRow mainCont = do
    header <- hBoxNew True 0
    boxPackStart mainCont header PackNatural 0

    fechaBut <- buttonNewWithLabel "Fecha"
    tipoBut <- buttonNewWithLabel "Tipo"
    methodBut <- buttonNewWithLabel "Metodo"
    numBut <- buttonNewWithLabel "n°"
    montoBut <- buttonNewWithLabel "Monto"
    descBut <- buttonNewWithLabel "Descripcion"

    let buttons = [fechaBut,tipoBut,methodBut,numBut,montoBut,descBut]
    boxPackStartGrow header buttons 0

mainTable :: VBox -> Programm -> IO ScrolledWindow
mainTable mainCont prog = do
    table <- newTable 
    boxPackStart mainCont table PackGrow 0

    appendCell table prog
    appendCell table prog

    return table
   
foot :: VBox -> IO ()
foot mainCont = do
    foot <- hBoxNew False 0
    boxPackEnd mainCont foot PackNatural 10

    eti1 <- labelNewWithMnemonic "Hora"
    eti2 <- labelNewWithMnemonic "nose"
    eti3 <- labelNewWithMnemonic "algo"
    eti4 <- labelNewWithMnemonic "Total"

    boxPackStartGrow foot [eti1,eti2,eti3,eti4] 0

