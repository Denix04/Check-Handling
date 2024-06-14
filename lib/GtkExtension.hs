module GtkExtension where

import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import System.Glib
import SignalHandlers
import Data
import DataManipulation
import Utilities

newMenuOption :: String -> [String] -> IO Expander
newMenuOption s ops = do
    expander <- expanderNewWithMnemonic s 
    menu <- menuBarNew
    menuBarSetPackDirection menu PackDirectionTtb
    appendOptions ops menu
    containerAdd expander menu
    return expander

    where
        appendOptions :: [String] -> MenuBar -> IO ()
        appendOptions [] _ = return ()
        appendOptions (x:xs) m = do
            item <- menuItemNewWithLabel x
            menuShellAppend m item
            appendOptions xs m

newCell :: Programm -> IO Cell
newCell prog = do
    cell <- hBoxNew False 0

    date <- entryNew
    opType <- entryNew
    opMethod <- entryNew
    opId <- entryNew
    opAmt <- entryNew
    desc <- entryNew

    boxPackStartGrow cell [date,opType,opMethod,opId,opAmt,desc] 0

    _ <- on date focusOutEvent $ dateCorroboration date
    _ <- on opType focusOutEvent $ opTypeCorroboration opType
    _ <- on opMethod focusOutEvent $ opMethodCorroboration opMethod
    _ <- on opId focusOutEvent $ opIdCorroboration opId opMethod
    _ <- on opAmt focusOutEvent $ opAmtCorroboration opAmt
    _ <- on desc focusOutEvent $ descCorroboration cell prog

    accounted' <- newIORef False
    let cell' = Cell cell date opType opMethod opId opAmt desc accounted'
    appendIORef (progCells prog) cell'
    return $ cell'

appendCell :: ScrolledWindow -> Programm -> IO ()
appendCell scroll prog = 
    containerGetChildren scroll >>= \vp ->
    viewportGetChilds vp >>= 
    viewportBoxesManagent >>= \vbox ->
    newCell prog >>= \c -> appendBox vbox c

    where
      viewportGetChilds vp = 
          concat <$> (mapM (containerGetChildren . castToContainer) vp)

      viewportBoxesManagent [] = 
          error "Main table not have been generated correctly"
      viewportBoxesManagent (x:_) = return $ castToVBox x

      appendBox box c = boxPackStart box (cell c) PackNatural 0

newTable :: IO ScrolledWindow
newTable = do
    scroll <- scrolledWindowNew Nothing Nothing
    box' <- viewportNew Nothing Nothing
    box <- vBoxNew True 0
    containerAdd scroll box'
    containerAdd box' box

    return scroll
