module GtkExtension where

import Control.Monad
import Graphics.UI.Gtk
import SignalHandlers

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

newCell :: IO HBox
newCell = do
    cell <- hBoxNew False 0

    date <- entryNew
    checkId <- entryNew
    typeOp <- entryNew
    amount <- entryNew
    desc <- entryNew

    boxPackStartGrow cell [date,checkId,typeOp,amount,desc] 0

    _ <- on date focusOutEvent $ dateCorroboration date
    _ <- on checkId focusOutEvent $ checkIdCorroboration checkId
    _ <- on typeOp focusOutEvent $ typeOpCorroboration typeOp
    _ <- on amount focusOutEvent $ amountCorroboration amount
    _ <- on desc focusOutEvent descCorroboration

    return cell

appendCell :: ScrolledWindow -> IO ()
appendCell scroll = 
    containerGetChildren scroll >>= 
    (\(viewPort:_) -> viewportChilds viewPort) >>= 
    (\(box:_) -> newCell >>= \c -> appendBox box c)

    where
      viewportChilds = containerGetChildren . castToContainer
      appendBox box c = boxPackStart (castToBox box) c PackNatural 0

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

    cel1 <- newCell

    boxPackStart box cel1 PackNatural 0

    return scroll

getDataTable :: ScrolledWindow -> IO [[String]]
getDataTable scroll =
    getEntriesTable scroll >>=
    getDataEntries

getEntriesTable :: ScrolledWindow -> IO [HBox]
getEntriesTable scroll = 
    containerGetChildren scroll >>= \viewPort ->
    obtainChildOf viewPort >>= \vBoxes ->
    obtainChildOf vBoxes >>= \hBoxes ->
    return $ fmap castToHBox hBoxes

    where 
        obtainChildOf :: [Widget] -> IO [Widget]
        obtainChildOf parent = 
            fmap concat (sequence $ 
                        map (containerGetChildren . castToContainer) parent)

getDataEntries :: [HBox] -> IO [[String]]
getDataEntries boxes = aux (pure []) boxes
    where
        aux :: IO [[String]] -> [HBox] -> IO [[String]]
        aux acc [] = acc
        aux acc (b:bs) = aux (concatM (getTextCell b) acc) bs

        concatM = liftM2 (:)

getTextCell :: HBox -> IO [String]
getTextCell b = 
    containerGetChildren b >>= \entrie ->
    sequence $ map (entryGetText . castToEntry) entrie >>= \texts ->
    return texts


-------------------------------------------------------
-- Cosas que hice para probar
-------------------------------------------------------

pedir :: Maybe String -> IO HBox
pedir s = do
    etiqueta <- labelNew s
    entrada <- entryNew
    boton <- buttonNewWithLabel "Enviar"
    _ <- on boton buttonActivated (buttonSetLabel boton "Enviado")

    box <- hBoxNew True 2
    boxPackStartDefaults box etiqueta
    boxPackStartDefaults box entrada
    boxPackStartDefaults box boton

    return box

salir :: ScrolledWindow -> IO ()
salir tabla = do
    datos <- getDataTable tabla
    putStrLn $ show datos
    mainQuit

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


