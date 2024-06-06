module GtkExtension where

import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import System.Glib
import SignalHandlers
import Data
import DataManipulation
import Utilities

data Cell = Cell {
    cell :: HBox,
    cellDate :: Entry,
    cellOpType :: Entry,
    cellOpMethod :: Entry,
    cellOpId :: Entry,
    cellOpAmt :: Entry,
    cellDescription :: Entry,
    accounted :: Bool }

type Cells = [Cell]

instance Eq Cell where
    c1 == c2 = cell c1 == cell c2

-----------------------------
-- Menu
-----------------------------

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

-----------------------------
-- Cells
-----------------------------

newCell :: IORef Registers -> IO Cell
newCell registers = do
    cell <- hBoxNew False 0

    date <- entryNew
    opType <- entryNew
    opMethod <- entryNew
    opId <- entryNew
    opAmt <- entryNew
    desc <- entryNew
    --widgetSetName desc "description"

    boxPackStartGrow cell [date,opType,opMethod,opId,opAmt,desc] 0

    _ <- on date focusOutEvent $ dateCorroboration date
    _ <- on opType focusOutEvent $ opTypeCorroboration opType
    _ <- on opMethod focusOutEvent $ opMethodCorroboration opMethod
    _ <- on opId focusOutEvent $ opIdCorroboration opId opMethod
    _ <- on opAmt focusOutEvent $ opAmtCorroboration opAmt
    _ <- on desc focusOutEvent $ descCorroboration cell registers

    --appendIORef
    return $ Cell cell date opType opMethod opId opAmt desc False

appendCell :: ScrolledWindow -> IORef Registers -> IO ()
appendCell scroll registers = 
    containerGetChildren scroll >>= \vp ->
    viewportGetChilds vp >>= 
    viewportBoxesManagent >>= \vbox ->
    newCell registers >>= \c -> appendBox vbox c

    where
      viewportGetChilds vp = 
          concat <$> (mapM (containerGetChildren . castToContainer) vp)

      viewportBoxesManagent [] = 
          error "Main table not have been generated correctly"
      viewportBoxesManagent (x:_) = return $ castToVBox x

      appendBox box c = boxPackStart box (cell c) PackNatural 0

-----------------------------
-- Table
-----------------------------

newTable :: IORef Registers -> IO ScrolledWindow
newTable registers = do
    scroll <- scrolledWindowNew Nothing Nothing
    box' <- viewportNew Nothing Nothing
    box <- vBoxNew True 0
    containerAdd scroll box'
    containerAdd box' box

    c1 <- newCell registers

    boxPackStart box (cell c1) PackNatural 0

    return scroll
