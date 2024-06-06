module Utilities where

import Graphics.UI.Gtk

split :: Char -> String -> [String]
split _ [] = []
split c s = [takeWhile (\x -> x /= c) s] ++ 
           split c (drop 1 (dropWhile (\x -> x /= c) s))

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
