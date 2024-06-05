module SignalHandlers where

import System.Glib
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import Data.IORef

import Data
import DataManipulation
import DataRetrieve
import CSV
import Functionalities

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
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . show) date
    return False

opTypeCorroboration :: Entry -> EventM EFocus Bool
opTypeCorroboration entry = liftIO $ do
    tOp <- (strToOpType . glibToString) <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . show) tOp
    return False

opMethodCorroboration :: Entry -> EventM EFocus Bool
opMethodCorroboration entry = liftIO $ do
    tOp <- (strToOpMethod . glibToString) <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . show) tOp
    return False

opIdCorroboration :: Entry -> EventM EFocus Bool
opIdCorroboration entry = liftIO $ do
    opId <- strToOpId <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (putStrLn . show) opId
    return False

opAmtCorroboration :: Entry -> EventM EFocus Bool
opAmtCorroboration entry = liftIO $ do
    amount <- strToAmount <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (putStrLn . show) amount
    return False

descCorroboration :: HBox -> IORef Registers -> EventM EFocus Bool
descCorroboration box registers = liftIO $ do
    regs <- listToRegister <$> getTextCell box
    either (notComplete box) (appendRegister registers) regs
    return False

    where
        notComplete box n = 
            containerGetChildren box >>= \boxs ->
            widgetGrabFocus $ boxs !! n

        appendRegister regs new =
            readIORef regs >>= \r -> writeIORef regs (new:r)

cellManipulation :: HBox -> IORef Registers -> Maybe Widget -> IO ()
cellManipulation box registers _ = do
    reg <- listToRegister <$> getTextCell box
    either (\_ -> putStrLn "No esta completa la casilla") (putStrLn . show) reg

quitProgram :: String -> IORef Registers -> IO ()
quitProgram path reg = do
    info <- readIORef reg
    (putStrLn . show) info
    save path reg
    mainQuit
