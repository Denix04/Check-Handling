module UI where

import Graphics.UI.Gtk
import Data.IORef
import GtkExtension
import SignalHandlers
import Data

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
    numBut <- buttonNewWithLabel "n°"
    tipoBut <- buttonNewWithLabel "Tipo"
    montoBut <- buttonNewWithLabel "Monto"
    descBut <- buttonNewWithLabel "Descripcion"

    let buttons = [fechaBut,numBut,tipoBut,montoBut,descBut]
    boxPackStartGrow header buttons 0


mainTable :: VBox -> IORef Registers -> IO ScrolledWindow
mainTable mainCont registers = do
    table <- newTable registers
    boxPackStart mainCont table PackGrow 0

    appendCell table registers
    appendCell table registers

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

