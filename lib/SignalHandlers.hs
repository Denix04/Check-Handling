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
import Utilities

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
    opMethod <- (strToOpMethod . glibToString) <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (entrySetText entry . show) opMethod
    return False

opIdCorroboration :: Entry -> Entry -> EventM EFocus Bool
opIdCorroboration entry method = liftIO $ do
    opId <- strToOpId <$> entryGetText entry
    opMethod' <- (strToOpMethod . glibToString) <$> entryGetText method
    either (\_ -> widgetGrabFocus entry) (testMethod entry opMethod') opId
    return False

    where
        testMethod :: Entry -> Either Int OpMethod -> Id -> IO ()
        testMethod entry (Right Check)  (Person _) = widgetGrabFocus entry
        testMethod _ _ id = return ()

opAmtCorroboration :: Entry -> EventM EFocus Bool
opAmtCorroboration entry = liftIO $ do
    amount <- strToAmount <$> entryGetText entry
    either (\_ -> widgetGrabFocus entry) (\_ -> return ()) amount
    return False

descCorroboration :: HBox -> Programm -> EventM EFocus Bool
descCorroboration box prog = liftIO $ do
    regs <- listToRegister <$> getTextCell box
    either (notComplete box) (complete box prog) regs
    return False

    where
        notComplete box n = 
            containerGetChildren box >>= \entries ->
            widgetGrabFocus $ entries !! n

        complete :: HBox -> Programm -> Register -> IO ()
        complete box prog reg = 
            findCell prog box >>= \cell ->
            maybe (return False) findIt cell >>= \x ->
            if x then updateProgramm prog reg else return ()

        findIt :: Cell -> IO Bool
        findIt cell = do
            acc <- readIORef $ accounted cell
            case acc of
                False -> 
                    writeIORef (accounted cell) True >> return True
                True -> return False

        updateProgramm :: Programm -> Register -> IO ()
        updateProgramm prog reg =
            appendIORef (progRegisters prog) reg >>
            calculateTotal prog >>
            --addToTotal prog reg >>
            readIORef (progTotalAmt prog) >>= \amt ->
            labelSetText (progTotalLabel prog) $ show amt

quitProgram :: String -> Programm -> IO ()
quitProgram path prog = do
    info <- readIORef (progRegisters prog)
    (putStrLn . show) info
    save path (progRegisters prog)
    mainQuit
