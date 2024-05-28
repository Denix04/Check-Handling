module GtkExtension where

import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import SignalHandlers
import Data

data Cell = Cell {
    cell :: HBox,
    dateEntry :: Entry,
    checkIdEntry :: Entry,
    typeOpEntry :: Entry,
    amountEntry :: Entry,
    descEntry :: Entry }
    
getDataEntry :: ContainerClass a => a -> IO [String]
getDataEntry con =
    containerGetChildren con >>=
    filterM (\wid -> (=="GtkEntry") <$> typeWidget wid) >>=
    (\x -> return $ mapText x) >>= 
    (\s -> sequence s)

    where 
        mapText = fmap (entryGetText . castToEntry)

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

newCell :: IO Cell
newCell = do
    cell <- hBoxNew False 0

    date <- entryNew
    checkId <- entryNew
    typeOp <- entryNew
    amount <- entryNew
    desc <- entryNew
    widgetSetName desc "description"

    boxPackStartGrow cell [date,checkId,typeOp,amount,desc] 0

    return Cell {
        cellBox = cell,
        dateEntry = date,
        chechIdEntry = checkId,
        typeOpEntry = typeOp,
        amountEntry = amount,
        descEntry = desc }

appendCell :: ScrolledWindow -> IORef Registers -> IO ()
appendCell scroll registers = 
    containerGetChildren scroll >>= 
    (\(viewPort:_) -> viewportChilds viewPort) >>= 
    (\(box:_) -> newCell >>= \c -> appendBox box c >> linkSignals c registers)

    where
      viewportChilds = containerGetChildren . castToContainer
      appendBox box c = boxPackStart (castToBox $ box) (cellBox c) PackNatural 0

-----------------------------
-- Table
-----------------------------

newTable :: IO ScrolledWindow
newTable = do
    scroll <- scrolledWindowNew Nothing Nothing
    box' <- viewportNew Nothing Nothing
    box <- vBoxNew True 0
    containerAdd scroll box'
    containerAdd box' box

    cell <- newCell

    boxPackStart box (cellBox cell) PackNatural 0

    return scroll

-----------------------------
-- Utilities
-----------------------------

typeWidget :: WidgetClass a => a -> IO String
typeWidget wid =
    widgetPath wid >>= (\path -> let (_,_,reversedPath) = path
    in return ((reverse . takeWhile (\x -> x /= '.')) reversedPath))

boxPackStartGrow :: (BoxClass self, WidgetClass child) => 
                    self -> [child] -> Int -> IO ()
boxPackStartGrow _ [] _ = return ()
boxPackStartGrow box (c:cs) pad = 
    boxPackStart box c PackGrow pad >>
    boxPackStartGrow box cs pad

linkSignals :: Cell -> IORef Registers -> IO ()
linkSignals cell registers = do

    _ <- on (date (cell)) focusOutEvent $ dateCorroboration date (cell)
    _ <- on (checkId (cell)) focusOutEvent $ checkIdCorroboration checkId (cell)
    _ <- on (typeOp (cell)) focusOutEvent $ typeOpCorroboration typeOp (cell)
    _ <- on (amount (cell)) focusOutEvent $ amountCorroboration amount (cell)
    _ <- on (desc (cell)) focusOutEvent descCorroboration
    _ <- on (cellBox (cell)) setFocusChild $ 
        cellManipulation (cellBox (cell)) registers
