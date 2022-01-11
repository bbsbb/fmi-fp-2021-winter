-- Exercise 1: Create a function that accepts a string and returns a list
-- of pairwise sums of the characters values within the string.
-- (assume even number of characters)
-- e.g. - "acbd" = [n1, n2] where
--                  n1 is the sum of a + c and b2 is the sum of b + d


import Data.Char

sumPairwise :: String -> [Int]
sumPairwise [] = []
sumPairwise [c] = [ord c]
sumPairwise (x:y:xs) = (ord x + ord y):sumPairwise xs


-- Exercise 2: Create a function that recieves a string and
-- a guardian character. The result is all the substrings enclosed
-- inbetween guardians.
-- e.g. "abzddaz23az" 'z' -> ["dda", "23a"]

-- -- You have a String
-- -- Read one character.
-- -- -- Is it a z?
-- -- -- -- Yes -> Are we opening or are we closing?
-- -- -- -- -- If opening, start with empty string, next char.
-- -- -- -- -- if closing, add current string to result, mark as closed.
-- -- -- -- No -> Are we opened? Add to current substring.
-- -- -- --       Not opened? Discard, continue next char.

extractString :: String -> Bool -> String -> [String] -> [String]
extractString [] _ _ result = result
extractString (c:xs) cutting current result
  | c == 'z' = if cutting
               then
                 extractString xs False "" result ++ [current]
               else
                 extractString xs True "" result
  | cutting = extractString xs cutting (current ++ [c]) result
  | otherwise = extractString xs cutting current result

chunkString :: String -> Char -> [String]
chunkString xs c = extractString xs False "" []



-- Exercise 3:
-- type Name = String
-- type Date = String
-- type Class = String
-- type Result = String
-- type Launched = Int

-- data Battle = Battle Name Date deriving Show
-- data Ship = Ship Name Class Launched deriving Show
-- data Outcome = Outcome Name Name Result deriving Show

-- data Outcome = Outcome {shipOne :: Name, shiptwo :: Name, final :: Result} deriving Show


-- type Database = ([Outcome], [Battle], [Ship])

-- outcomes :: [Outcome]
-- outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

-- battles :: [Battle]
-- battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]

-- ships :: [Ship]
-- ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]

-- database :: Database
-- database = (outcomes, battles, ships)

-- getName :: Outcome -> Name
-- getName (Outcome n _ _ ) = (head os).shipOne

-- getSunk :: Database -> Name
-- getSunk (os, bs, ss) = (\ (Outcome n _ _) -> n) $ head os

-- As homework, solve with treeTraverse + higher order function TO BE FANCY.
data BTree = Nil | Node (Float, Float) BTree BTree

-- First contained within 2nd
nodeValue :: BTree -> (Float, Float)
nodeValue (Node v _ _) = v


intervalCmp :: (Float, Float) -> (Float, Float) -> Bool
intervalCmp (x1, y1) (x2, y2) = x1 >= x2 && y1 <= y2


orderedTree = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) Nil Nil)
                               (Node (4.0,9.0) Nil Nil))
              (Node (2.0,12.0) Nil
                (Node (1.0,15.0) Nil Nil))

isIntervalBst :: BTree -> Bool
isIntervalBst Nil = True
isIntervalBst (Node _ Nil Nil) = True
isIntervalBst (Node _ Nil _) = True
isIntervalBst (Node _ _ Nil) = True
isIntervalBst (Node v l r) = (intervalCmp (nodeValue l) v) && (intervalCmp v (nodeValue r)) && isIntervalBst l && isIntervalBst r
