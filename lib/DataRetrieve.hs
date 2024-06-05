module DataRetrieve where

import Graphics.UI.Gtk
import Control.Monad
import Data
import DataManipulation

getDataTable :: ScrolledWindow -> IO [[String]]
getDataTable scroll =
    getEntriesTable scroll >>= getDataEntries

getEntriesTable :: ScrolledWindow -> IO [HBox]
getEntriesTable scroll = 
    containerGetChildren scroll >>= \viewPort ->
    obtainChildOf viewPort >>= \vBoxes ->
    obtainChildOf vBoxes >>= \hBoxes ->
    return $ fmap castToHBox hBoxes

    where 
        obtainChildOf parents = concat <$> forM parents getChildrens

        getChildrens = containerGetChildren . castToContainer

getDataEntries :: [HBox] -> IO [[String]]
getDataEntries boxes = forM boxes getTextCell

getTextCell :: HBox -> IO [String]
getTextCell b = containerGetChildren b >>= mapM getText
    where getText = entryGetText . castToEntry

compareHbox :: ScrolledWindow -> IO [Bool]
compareHbox scroll =
    getEntriesTable scroll >>= \xs -> return $ [ x == y | x <- xs ,y <- xs] 
