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
        Left _ -> widgetGrabFocus entry
        Right date -> entrySetText entry $ show date
    return False

checkIdCorroboration :: Entry -> EventM EFocus Bool
checkIdCorroboration entry = liftIO $ do
    checkId <- strToCheckId <$> entryGetText entry
    case checkId of
        Left _ -> widgetGrabFocus entry
        Right num -> putStrLn $ show num
    return False

typeOpCorroboration :: Entry -> EventM EFocus Bool
typeOpCorroboration entry = liftIO $ do
    typeOp <- (guesTypeOp . glibToString) <$> entryGetText entry
    case typeOp of
        Left _ -> widgetGrabFocus entry
        Right tOp -> entrySetText entry $ typeOpToStr tOp
    return False


amountCorroboration :: Entry -> EventM EFocus Bool
amountCorroboration entry = liftIO $ do
    amount <- strToAmount <$> entryGetText entry
    case amount of
        Left _ -> widgetGrabFocus entry
        Right num -> putStrLn $ show num
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

        appendRegister :: IORef Registers -> Register -> IO ()
        appendRegister register new = 
            appendIORef register new >>
            readIORef register >>= (putStrLn . show)

        appendIORef :: IORef Registers -> Register -> IO ()
        appendIORef regs n =
            readIORef regs >>= \registers -> writeIORef regs (n:registers)


cellManipulation :: HBox -> IORef Registers -> Maybe Widget -> IO ()
cellManipulation box registers _ = do
    reg <- listToRegister <$> getTextCell box
    either (\_ -> putStrLn "No esta completa la casilla") (putStrLn . show) reg

quitProgram :: IORef Registers -> IO ()
quitProgram reg =
    readIORef reg >>= putStrLn . show >>
    mainQuit

