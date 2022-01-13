-- Exercise LAST (hopefully)

-- Exercise 6 from prep:
-- -- You get a function.
-- -- The parameter is a list of functions of 1 argument.
-- -- Return the max value any of those functions emits.


maxHelper :: (Ord a, Num a)  => [(a -> a)] -> (a -> a) -> a -> (a -> a)
maxHelper [] selection _ = selection
maxHelper (f:rest) selection input
  | (f input) > (selection input) = maxHelper rest f input
  | otherwise = maxHelper rest selection input


--maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
--maximize xs = maxHelper [(\ x -> x)] (head xs)

maximize :: [(Int -> Int)] -> (Int -> Int)
maximize (f:rest) = (\input -> (maxHelper rest f input) input)

maximizeFold :: [(Int -> Int)] -> (Int -> Int)
maximizeFold (f:rest) = (\input -> (foldl (\highest current -> if (current input) > (highest input)
                                                               then
                                                                 current
                                                               else
                                                                 highest) f rest)
                                   input)







sumTwo :: Int -> Int -> Int
sumTwo a b = a + b

sumFive :: (Int -> Int)
sumFive = sumTwo 5

sumFiveOther :: (Int -> Int)
sumFiveOther = (\ input -> sumTwo input 5)

-- Exercise 7:

-- -- We accept two functions and an integer inteval.
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g start end = foldl (\isInverse n -> isInverse && n == (f (g n)) && n == (g (f n))) True [start..end]


--Who cares about dimensions.
data Coordinates a = Point a a deriving Show

-- Create a function that returns the euclidian distance between two points.
-- sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Haskell has partial application by default.
-- Calling any function of N arguments with n-1 arguments returns a ONE argument function.

eucDistance :: (Floating a) => Coordinates a -> Coordinates a -> a
eucDistance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

-- ^, **,



-- Write a function that verifies if a word is encoded within a treeeeeee

catTree = (Node 'c'
            Nil
            (Node 'a'
             (Node 'd' Nil Nil)
             (Node 't' Nil Nil)))

trickyCatTree = (Node 'c'
                  Nil
                  (Node 'a'
                   (Node 'd' Nil Nil)
                   (Node 'b'
                     Nil
                     (Node 'a'
                       (Node 't' Nil Nil)
                       (Node 'c' Nil Nil)))))



data BTree a = Nil | Node a (BTree a) (BTree a)

isEncodedRepeat :: BTree Char -> String -> Bool
isEncodedRepeat _ [] = True
isEncodedRepeat Nil _ = False
isEncodedRepeat (Node ch l r) (x:xs) = ch == x && (isEncodedRepeat l xs || isEncodedRepeat r xs)

isEncoded :: BTree Char -> String -> Bool
isEncoded _ [] = True
isEncoded Nil _ = False
isEncoded (Node ch l r) s = isEncodedRepeat (Node ch l r) s || isEncoded l s || isEncoded r s
