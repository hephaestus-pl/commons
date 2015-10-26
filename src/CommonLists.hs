module CommonLists(
 concatBefore,
 concatAfter, 
 concatAround, 
 precedence, 
 endsWith
) where 

concatBefore :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatBefore _ _ [] = []
concatBefore f ls (x:xs) = 
 if f x 
  then (ls ++ [x]) ++ (concatBefore f ls xs) 
  else x : (concatBefore f ls xs)

concatAfter :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatAfter _ _ [] = []
concatAfter f ls (x:xs) = 
 if f x
  then x : (ls ++ xs)
  else x : (concatAfter f ls xs)

concatAround :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> [a] -> [a]
concatAround _ _ _ [] = []
concatAround match cp ls (x:xs) = 
 if match x 
  then concat [proceed cp l (x:xs) | l <- ls]
  else x : concatAround match cp ls xs
 where 
  proceed c l x = if (c l) then x else [l]

-- this function is useful for defining 
-- precedences in a list that could not be ordered 
-- by just defining its basic type as an instance of the 
-- Order class.
--
-- examples:
-- 
--  precedence [(a2, a1)] [a5, a3, a1, a2] = [a5, a3, a2, a1]
--  precedence [(a2, a1), (a1,a5)]) [a5, a3, a2, a1] = 

precedence :: Eq a => [(a,a)] -> [a] -> [(a,Integer)]
precedence _ [] = []
precedence ps ls = 
 let 
   ls' = [(l,0) | l <- ls]
 in precedence' ps ls'

precedence' []  ls = [l | l<- ls]
precedence' (p:ps) ls = 
 let 
  px = [x | x <- ls, fst p == fst x]
  py = [y | y <- ls, snd p == fst y]
 in 
   case (px,py) of
    ([x],[y]) -> precedence' ps [if (fst y == fst l) then (fst y, (+1) (snd x)) else l | l <- ls]
    otherwise -> ls

endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith key list = key == (drop n list) 
 where n = (length list) - (length key)