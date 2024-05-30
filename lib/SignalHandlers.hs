module SignalHandlers where

import System.Glib
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import Data.IORef

import Data
import DataManipulation
import DataRetrieve

shortCutsManage :: WidgetClass object => object -> IO (ConnectId object)
shortCutsManage window = 
        window `on` keyPressEvent $ do
            nose <- glibToString <$> eventKeyName
            case nose of
                "n" -> do 
                    --liftIO $ appendCell window
                    --liftIO $ buttonSetLabel tabla "Funciona??"
                    liftIO $ putStrLn nose
                    return False
                "k" -> liftIO $ mainQuit >>
                    return True
                _ -> liftIO $ putStrLn nose >>
                    return False


dateCorroboration :: Entry -> EventM EFocus Bool
dateCorroboration entry = liftIO $ do
    date <- (strToDate . glibToString) <$> entryGetText entry
    case date of
        Nothing -> widgetGrabFocus entry
        Just date -> entrySetText entry $ show date
    return False

checkIdCorroboration :: Entry -> EventM EFocus Bool
checkIdCorroboration entry = liftIO $ do
    checkId <- strToNumber <$> entryGetText entry :: IO (Maybe Int)
    case checkId of
        Nothing -> widgetGrabFocus entry
        Just num -> putStrLn $ show num
    return False

typeOpCorroboration :: Entry -> EventM EFocus Bool
typeOpCorroboration entry = liftIO $ do
    typeOp <- (guesTypeOp . glibToString) <$> entryGetText entry
    case typeOp of
        Nothing -> widgetGrabFocus entry
        Just tOp -> entrySetText entry $ typeOpToStr tOp
    return False


amountCorroboration :: Entry -> EventM EFocus Bool
amountCorroboration entry = liftIO $ do
    amount <- strToNumber <$> entryGetText entry :: IO (Maybe Double)
    case amount of
        Nothing -> widgetGrabFocus entry
        Just num -> putStrLn $ show num
    return False

descCorroboration :: EventM EFocus Bool
descCorroboration = do
    liftIO $ putStrLn "Chau Description"
    return False

cellManipulation :: HBox -> IORef Registers -> Maybe Widget -> IO ()
cellManipulation box registers wid = do
    mayInfo <- listToRegister <$> getTextCell box
    case mayInfo of
        Just info -> putStrLn $ show info
        Nothing -> putStrLn "No esta completa la casilla"

cellManipulation' :: HBox -> IORef Registers -> Maybe Widget -> IO ()
cellManipulation' box registers wid = do
    case wid of
        Just widget -> do
            isDesc <- isDescription widget
            case isDesc of
                True ->
                    listToRegister <$> getTextCell box >>=
                    maybe (putStrLn "No esta completa la casilla")
                          (putStrLn . show)
                _ -> putStrLn "No es una descripcion"
        _ -> putStrLn "No se recivio un widget"

    where
        isDescription wid =
            widgetGetName wid >>=  
            return . ("description" ==)
    
cellManipulation'' :: HBox -> IORef Registers -> EventM EFocus Bool
cellManipulation'' box registers = liftIO $ do
    reg <- listToRegister <$> getTextCell box
    maybe (putStrLn "No esta completa la casilla") (putStrLn . show) reg
    return False

focusInManagent :: HBox -> EventM EFocus Bool
focusInManagent box = do
    widgets <- liftIO $ containerGetChildren box
    tryEvent $ giveFocusChildren widgets
    return False

    where 

        giveFocusChildren :: [Widget] -> EventM any ()
        giveFocusChildren [] = stopEvent 
        giveFocusChildren (x:xs) =
            (liftIO $ widgetGrabFocus x) >>
            giveFocusChildren xs


quitProgram :: ScrolledWindow -> IORef Registers -> IO ()
quitProgram tabla reg = do
    datos <- strToRegisters <$> getDataTable tabla

    writeIORef reg datos 
    registros <- readIORef reg
    putStrLn $ show registros

    mainQuit

