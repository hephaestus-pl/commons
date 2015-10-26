module CommonStrings(
 split,
 splitAndRemoveBlanks,
 splitAndTrim,
 trim, 
 removeLast,
 existWord, 
 replaceString, 
 parseString, 
 unparseString, 
 parenthesize
)where 

import Data.List

split :: Char -> String -> [String]
split c "" = [""] 
split c s = 
 if any (== c) s then 
  (delete c (takeWhile (/= c) s)) : split c (tail (dropWhile (/= c) s))
 else [s]
 
splitAndRemoveBlanks :: Char -> String -> [String] 
splitAndRemoveBlanks c s = [filter (/= ' ') x | x <- (split c s)] 

splitAndTrim :: Char -> String -> [String]
splitAndTrim ch str = [trim x | x <- (split ch str)]
 
trim :: String -> String
trim str 
 | (last str) == ' ' = trim (removeLast str)
 | (head str) == ' ' = trim (tail str)
 | otherwise = str
 
removeLast :: String -> String
removeLast (h:t) 
 | (length t) == 1 = []
 | otherwise = h:(removeLast t)

existWord:: String -> String -> Bool 
existWord w s  = 
 let ws = parseString s
 in  w `elem` ws

replaceString :: String -> String -> String -> String
replaceString old new str = 
 let ws = parseString str
 in unparseString (replaceString' old new ws)
 where 
  replaceString' o n [] = []
  replaceString' o n (w:ws) = (if (w == o) then n else w) : replaceString' o n ws

parseString :: String -> [String]
parseString s  = parseString' (s, "", [])

parseString' :: (String, String, [String]) -> [String]
parseString' ("", s2, ss) = (ss ++ [s2])
parseString' ((x:xs), s2, ss) = 
   if x `elem` [',', '.', ' ', '-'] 
    then parseString' (xs, "", (ss ++ [s2]) ++ [(x:"")])
    else parseString' (xs, s2 ++ (x:""), ss)

unparseString :: [String] -> String 
unparseString xs = concat xs

parenthesize :: [String] -> String
parenthesize [] = "-" 
parenthesize ss = "(" ++ parenthesize' ss ++ ")" 
 where 
  parenthesize'  [] = ""
  parenthesize' [l] = l 
  parenthesize' (l:ls) = l ++ ", " ++ parenthesize' ls
