module SignalHandlers where

import System.Glib
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import Data
import DataManipulation

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
        Just date -> entrySetText entry $ dateToStr date
    return False

checkIdCorroboration :: Entry -> EventM EFocus Bool
checkIdCorroboration entry = liftIO $ do
    checkId <- strToNumber <$> entryGetText entry :: IO (Maybe Double)
    case checkId of
        Nothing -> widgetGrabFocus entry
        Just num -> putStrLn $ show num
    return False

typeOpCorroboration :: Entry -> EventM EFocus Bool
typeOpCorroboration entry = liftIO $ do
    typeOp <- (guesTypeOp . glibToString) <$> entryGetText entry
    case typeOp of
        None -> widgetGrabFocus entry
        _ -> entrySetText entry $ typeOpToStr typeOp
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
