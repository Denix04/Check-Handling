module Utilities where

import Data.Char(toLower)
import Graphics.UI.Gtk
import Data.IORef

-- Base
split :: Char -> String -> [String]
split _ [] = []
split c s = [takeWhile (\x -> x /= c) s] ++ 
           split c (drop 1 (dropWhile (\x -> x /= c) s))

separateBy :: String -> [Char] -> [String]
separateBy s sep = aux s sep [] []
    where
        aux :: [Char] -> String -> String -> [String] -> [String]
        aux sep [] acc ss = ss ++ [acc]
        aux sep (x:xs) acc ss
            | x `elem` sep &&  acc == [] = aux sep xs [] ss
            | x `elem` sep = aux sep xs [] (ss ++ [acc])
            | otherwise  = aux sep xs (acc ++ [x]) ss

strToNumber ::Read a => String -> Maybe a
strToNumber s = 
    case reads s of
        [(n,"")] -> Just n
        _ -> Nothing

catEithers :: [Either a b] -> [b]
catEithers (x:xs) =
    case x of
        Left _ -> catEithers xs
        Right e -> e:catEithers xs

guessConstructor :: Show a => String -> [a] -> Maybe a
guessConstructor s xs = (mostSimilar . (coincidences xs) . toLowerCase) s
    where
        toLowerCase = map toLower

        coincidences :: Show a => [a] -> String -> [(a, Int)]
        coincidences xs s = zip xs $ map ((count 0 s) . toLowerCase . show) xs
            where 
                count acc _ [] = acc
                count acc [] _ = acc
                count acc (s:ss) (x:xs) 
                    | s == x = count (acc+1) ss xs
                    | otherwise = count (acc) ss xs

        mostSimilar :: [(a, Int)] -> Maybe a
        mostSimilar xs = aux 0 Nothing xs
            where 
                aux _ return [] = return
                aux acc return ((typ,cont):rest)
                    | cont > acc = aux cont (Just typ) rest
                    | otherwise = aux acc return rest
guessConstructor _ _ = Nothing

-- Gtk
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

-- IORef
appendIORef :: IORef [a] -> a -> IO ()
appendIORef rxs x =
    readIORef rxs >>= \xs -> writeIORef rxs (x:xs)
