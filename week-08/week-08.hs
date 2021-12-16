-- Week 08
--

-- Collections:

-- Add 1 in front of collection
-- 1:[2,3]

-- Append
-- [2,3] ++ [1]

-- Drop
-- drop 1 [1,2,3] ->> [2,3]

-- Take + Range
-- take 3 [1..] ->> [1,2,3]


-- Sorting.
-- -- Bubble sort <-- should be easier.
-- -- Selection sort
-- -- Insertion sort
-- -- Merge sort <-- Easier for me.
-- -- -- Divide & conquer alg.
-- -- -- Split always in half. When you arrive at two lists of 1 element, you have two sorted lists.
-- -- -- Merge them. Now you have two sorted lists. Merge them. ...
-- -- Quick sort


-- Exericise 2: Right a merge function - Accept two SORTED lists, return a single list in order.
--myMerge [1,3,4] [2,5,7] ->> [1,2,3,5,7]
-- myMerge :: [Integer] -> [Integer] -> [Integer]
-- myMerge xs [] = xs
-- myMerge [] ys = ys
-- myMerge (x:xs) (y:ys)
--   | x <= y = x:myMerge xs (y:ys)
--   | otherwise = y:myMerge (x:xs) ys



-- Abstract the type away.
-- -- Interface.
-- --
-- myMerge :: Sravnimo -> Sravnimo -> Sravnimo
-- -- Word we are looking for is Generics.

--Haskell has TYPECLASSES. Interface + generic + Steroids
-- Ord - built in interface for ORDERING
-- Num - numerical
myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge xs [] = xs
myMerge [] ys = ys
myMerge (x:xs) (y:ys)
  | x <= y = x:myMerge xs (y:ys)
  | otherwise = y:myMerge (x:xs) ys


-- Exercise 2: Implement the rest of merge-sort. Create a function that accepts an UNORDED list and using
-- the merge & split tactic described above + myMerge returns a sorted list.
-- myMergeSort [3, 1]
-- -> [3] [1] => myMerge => [1,3]
-- myMergeSort [1,7,5,2]
-- -> [1,7] [5,2] -> [1] [7] -> [1,7], [5] [2] -> [2, 5],  [1,7] [2,5] -> [1,2,5,7]

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



destructureTuple  :: [Integer] -> [[Integer]]
destructureTuple xs = result where
  (a,b) = splitAt 2 xs
  result = [a,b]


-- What is list comprehension?
-- -- [ELEMENT THAT WILL BE IN THE LIST | THE SHIT I AM TRAVERSING, THE CONDITION THAT DETERMINES IF SOMETHING IS IN OR NOT]

-- Exercise 2: Return a list of all divisors of a number.
-- --
--myDivisors 14 -> [2,7]
myDivisors :: Integer -> [Integer]
myDivisors n = [x | x <- [2..(n-1)], n `mod` x == 0]


-- Exercise 3: Write isPrime with list comprehension

isPrime :: Int -> Bool
isPrime n = length [m | m <- [2..(div n 2)], mod n m == 0] == 0



-- Exercise 4: Every even integer is the sum of two primes. Find em.
-- 8 = 3 + 5 etc.
partialPrimes :: Int -> (Int, Int) -- [Int]
partialPrimes n = head [(x, n - x) | x <- [2..n], isPrime x && isPrime (n - x)]
