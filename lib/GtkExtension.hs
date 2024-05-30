module GtkExtension where

import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import SignalHandlers
import Data

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

newCell :: IORef Registers -> IO HBox
newCell registers = do
    cell <- hBoxNew False 0
    widgetSetCanFocus cell True

    date <- entryNew
    checkId <- entryNew
    typeOp <- entryNew
    amount <- entryNew
    desc <- entryNew
    widgetSetName desc "description"

    boxPackStartGrow cell [date,checkId,typeOp,amount,desc] 0

    _ <- on date focusOutEvent $ dateCorroboration date
    _ <- on checkId focusOutEvent $ checkIdCorroboration checkId
    _ <- on typeOp focusOutEvent $ typeOpCorroboration typeOp
    _ <- on amount focusOutEvent $ amountCorroboration amount
    _ <- on desc focusOutEvent descCorroboration

    --_ <- on cell setFocusChild $ cellManipulation' cell registers
    _ <- on cell focusInEvent $ focusInManagent cell
    _ <- on cell focusOutEvent $ cellManipulation'' cell registers

    return cell

appendCell :: ScrolledWindow -> IORef Registers -> IO ()
appendCell scroll registers = 
    containerGetChildren scroll >>= 
    (\(viewPort:_) -> viewportChilds viewPort) >>= 
    (\(box:_) -> newCell registers >>= \c -> appendBox box c)

    where
      viewportChilds = containerGetChildren . castToContainer
      appendBox box c = boxPackStart (castToBox box) c PackNatural 0

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

    cel1 <- newCell registers

    boxPackStart box cel1 PackNatural 0

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

