module  ShortCuts where

import System.Glib
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import GtkExtension

shortCutsManage :: WidgetClass object => object -> Table -> IO (ConnectId object)
shortCutsManage window tabla = 
        window `on` keyPressEvent $ do
            nose <- glibToString <$> eventKeyName
            case nose of
                "n" -> do 
                    liftIO $ appendCell tabla 2
                    --liftIO $ buttonSetLabel tabla "Funciona??"
                    liftIO $ putStrLn nose
                    return False
                "k" -> liftIO $ mainQuit >>
                    return True
                _ -> liftIO $ putStrLn nose >>
                    return False
