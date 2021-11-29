-- Haskell
-- Statically typed - Types are defined at compilation time.
-- Strongly typed - Absolutely no type conversion.
-- Lazy by default - Computation is executed only when needed.
-- All functions are curried by default - Any function of N arguments, can be invoced with N-M arguments.
-- Interfaces - Enforce behaviour instead of structure.
-- Composition over inheritence <-- Google this.
-- More than interfaces -> Typeclasses.
-- Algebraic Data Types -> You will probably like it.
-- Pattern matching


-- Exercise 1: Write a function that sums 3 numbers.

sumNumbers :: Integer -> Integer -> Integer -> Integer -- Signature
sumNumbers x y z = x + y + z -- Body


-- Exercise 2: Function that counts all digits in a numbers
-- countDigits 1234 -> 4
-- Solve with recursion
-- if < 10 => 1
-- If not  return 1 + n / 10
countDigits :: Integer -> Integer
countDigits n = if n < 10 then 1
                else 1 + countDigits (quot n 10)


countDigitsGuards n
  | n < 10 = 1
  | otherwise = 1 + countDigitsGuards (quot n 10)

-- Conditionals:
-- We have IF.
-- We have GUARDS
-- We have pattern matching


-- Exercise 3:
-- Create a function that accepts a paramenter integer and returns all integers larger than it.
-- greaterThanN 11 -> [12, 13, 14, 15..]

greaterThanN :: Integer -> [Integer]
greaterThanN n = n : greaterThanN(n + 1)


-- Exercise 4:
-- Write factorial WITH PATTERN MATCHING.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Exercise 4:
-- Spoilers: Strings are still an array of chars.

iAmString :: String -> String
iAmString s = s

jeSuisString :: [Char] -> [Char]
jeSuisString s = s


-- Exercise 5:
-- Write a function that determines if a given string is a prefix of another
-- isPrefix "abc" "ab" -> true
-- isPrefix "acb" "ab" -> false
-- Write it with PATTERN MATCHING
-- Google pattern matching on lists.
-- -- If first string is empty => false
-- -- if second string is empty => true
-- -- Recursion, I guess?


isPrefix :: String -> String -> Bool
isPrefix _ [] = True
isPrefix [] _ = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys


-- Exercise Next: Write a function that checks if a string is CONTAINED within another.
-- isContained "bvbaddsa" "add" -> True
-- isContained "bvbadcsa" "add" -> False
-- Solving: Just,
-- -- left is empty => False
-- -- right is empty => True
-- -- -- if it's prefix => True
-- -- -- recur tail left + same right
isContained :: [Char] -> [Char] -> Bool
isContained _ [] = True
isContained [] _ = False
isContained (x:xs) ys
  | isPrefix (x:xs) ys = True
  | isContained xs ys = True
  | otherwise = False
