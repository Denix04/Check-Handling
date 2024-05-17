module SignalHandlers where

import System.Glib
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk

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


entryCorr :: EventM EFocus Bool
entryCorr = do
    liftIO $ putStrLn "Chau Fecha"
    return False

checkIdCorr :: EventM EFocus Bool
checkIdCorr = do
    liftIO $ putStrLn "Chau n° cheque"
    return False

typeOpCorr :: EventM EFocus Bool
typeOpCorr = do
    liftIO $ putStrLn "Chau Type Operation"
    return False


amountCorr :: EventM EFocus Bool
amountCorr = do
    liftIO $ putStrLn "Chau Amount"
    return False

descCorr :: EventM EFocus Bool
descCorr = do
    liftIO $ putStrLn "Chau Description"
    return False
