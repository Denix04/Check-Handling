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
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . show) date
    return False

checkIdCorroboration :: Entry -> EventM EFocus Bool
checkIdCorroboration entry = liftIO $ do
    checkId <- strToCheckId <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (putStrLn . show) checkId
    return False

typeOpCorroboration :: Entry -> EventM EFocus Bool
typeOpCorroboration entry = liftIO $ do
    tOp <- (guesTypeOp . glibToString) <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . typeOpToStr) tOp
    return False

amountCorroboration :: Entry -> EventM EFocus Bool
amountCorroboration entry = liftIO $ do
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

quitProgram :: IORef Registers -> IO ()
quitProgram reg = readIORef reg >>= putStrLn . show >> mainQuit
