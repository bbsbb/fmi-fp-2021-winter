import Data.Char

-- Exercise 1:
-- https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
-- tl;dr:
-- The transform is done by sorting all the circular shifts
-- of a text in lexicographic order and by extracting the last column
-- and the index of the original string in the set of sorted permutations of S.
-- ^BANANA| -> BNN^AA|A
-- What we learned: There is a function init that returns all elements except the
-- last one.
-- Lisp/Scheme dialects: butlast, Racket: drop-right

rotateWord :: String -> String
rotateWord [] = []
rotateWord word = last word:init word

rotateWithRate :: String -> Int ->String
rotateWithRate word n = drop affectedChars word ++ take affectedChars word where
  affectedChars = length word - n

allRotations :: String -> [String]
allRotations phrase = [rotateWithRate phrase n | n <- [0..(length phrase - 1)]]


myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge xs [] = xs
myMerge [] ys = ys
myMerge (x:xs) (y:ys)
  | x <= y = x:myMerge xs (y:ys)
  | otherwise = y:myMerge (x:xs) ys

myMergeSort :: (Ord a) => [a] -> [a]
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort xs = myMerge (myMergeSort ys) (myMergeSort zs) where
  (ys, zs) = splitAt (quot (length xs) 2) xs


bwTransform :: String -> String
bwTransform phrase = [ last phraseRotation | phraseRotation <- myMergeSort $ allRotations phrase]


--- Going into higher order functions.

-- Functions that treat other functions as values.

-- map
-- it iterates

incrementList :: [Int] -> [Int]
incrementList xs = map (\ n -> 1 + n) xs

-- Spoilers: doesn't work
-- incrementListTwo :: [Int] -> [Int]->[Int]
-- incrementListTwo xs ys = map (\ n m-> m + n) xs ys


-- filter
-- removes values from a list based on a predicate

noAsPlz :: [String] -> [String]
noAsPlz words = filter (\ word -> last word /= 'a') words

-- fold == reduce
-- It's a function that produces a new value out of a collection
-- Read it for real this time:
-- 1. https://wiki.haskell.org/Fold
-- 2. https://wiki.haskell.org/Foldr_Foldl_Foldl'

sumList :: [Int] -> Int
sumList xs = foldl (\acc n -> acc + n) 0 xs

sumListR :: [Int] -> Int
sumListR xs = foldr (\n acc -> acc + n) 0 xs


-- Exercise: Aoc 2018 / 05  https://adventofcode.com/2018/day/5
-- tl;dr: Side by side characters react based on rules. Reduce a string
-- to a smaller string.
-- example:
-- dabA|cC|aCBAcCcaDA ->  dab|Aa|CBAcCcaDA -> dabCBA|cC|caDA -> dabCBAcaDA <-- This is final
--

-- Questions:
-- -- How do we determine if two characters can react?
              -- ASCII diff should be equal to something...toi koda shte si go smetne.
-- -- How do we remove from the string?
              -- Nope. Don't. You build a new one. You never need to sub string.
              -- Tail/Head is enough
-- -- How do we traverse the string?


-- Who cares if tehre is shorter that's a 6/6. I like this more.
-- Vasi says this breaks, cause she doesn't know math.
canReact :: Char -> String -> Bool
canReact ch1 [] = False
canReact ch1 s = abs(ord ch1 - ord ch2) == ord 'a' - ord 'A' where
  ch2 = head s

-- Same but different.
canReactBool :: Char -> Char -> Bool
canReactBool ch1 ch2 = ch1 /= ch2 && (toLower ch1 == toLower ch2)

foldP :: String -> String
foldP p = foldr (\ ch newPolymer -> if canReact ch newPolymer then tail newPolymer else ch:newPolymer) [] p

-- List gets reversed
foldPL :: String -> String
foldPL p = reverse $ foldl (\ newPolymer ch -> if canReact ch newPolymer then tail newPolymer else ch:newPolymer) [] p
