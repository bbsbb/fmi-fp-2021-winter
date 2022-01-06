-- Algebraic Data Types
-- -- Product Types
-- -- --

-- Haskell Records: A person
data Person = Person {age :: Int, name :: String} deriving Show

isBoyan :: Person -> Bool
isBoyan Person {name = person_name} = person_name == "boyan"


-- -- Sum types
-- -- -- A type represented by an exhaustive list of values.

-- What is a perimeter? It's a number. Spoilers: Numbers are not a sum type. Spoilers: Not a product type either.


-- Sum Type Examples
-- -- -- Days of the Week - 1,2,3,4,5,6,7
-- -- -- Months of the year -
-- -- -- Colors -
-- -- -- Alphabet -
-- -- -- Clock hours -

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

thankGodItsFriday :: DayOfWeek -> Bool
thankGodItsFriday Friday = True
thankGodItsFriday Monday = False



-- Google "Expression Problem" -> There is a single good article on it that explains it well. Find it and read it. Good luck!
-- -- There are two approaches to designing polymorphic software. One creates additional cost to introduce a type and the other
-- -- creates cost in introducing behavior.


-- Elizabeth's Shapes - This is a sum type. it is ONE of the things inside, it's not it's own thing.
-- Works with any kind of digit.
data Shape a = Square a | Rectangle a a | Triangle a a a deriving Show

-- Typeclassess
perimeter :: (Floating a) => Shape a -> a
perimeter (Square side) = side * 4
perimeter (Rectangle sideA sideB) = sideA * 2 + sideB * 2
perimeter (Triangle a b c) = a + b + c


area :: (Floating a) => Shape a -> a
area (Square side) = side * side
area (Rectangle b c) = b * c
area (Triangle a b c) = sqrt(p * (p -a) * (p - b) * (p - c)) where
  p = perimeter (Triangle a b c) / 2


-- So, adding types vs adding behavior has a trade off. Think about it when desiging.

-- Recursive Algebraic Data types

--data BTree = Nil | Node Int BTree Btree

data BTree a = Nil | Node a (BTree a) (BTree a)

--Rectangle (Int) (Int) (Int)
--Node (Int) (Btree Int) (Btree Int)
-- What is the depth of a B5Ainary tree?
--    4 - Depth is 3
--   /  \
--  2    3
--      / \
--     1  Nil

--
depthTree :: BTree a -> Int
depthTree Nil = 0
depthTree (Node v left right) = 1 + max (depthTree left) (depthTree right)

-- How many nodes with values does a tree have?
-- aTree should have 4 nodes with values.
countLeaves :: BTree a -> Int
countLeaves Nil = 0
countLeaves (Node v left right) = 1 + (countLeaves left) + (countLeaves right)


-- How many ways are there to traverse a tree?
-- -- In Order, Post Order aaaand ..............Pre order
-- In Order -> Left Root Right
-- Pre Order -> Root Left Right
-- Post Order -> Left Right Root

aTree = (Node 10
          (Node 5
            Nil
            (Node 7 Nil Nil))
          (Node 15 Nil Nil))
-- Write a function that returns an in order path of the tree.
-- aTree -> [5, 7, 10, 15]
-- What kind of tree is aTree? Binary Search Tree.

-- In Order
treeTraverse :: BTree a -> [a]
treeTraverse Nil = []
treeTraverse (Node v left right) = (treeTraverse left) ++ [v] ++ (treeTraverse right)


bTree = (Node 10
          (Node 5
            Nil
            (Node 7 Nil Nil))
          (Node 15
            (Node 13 Nil Nil)
            Nil))

-- Tree Equality.
-- Write a function that returns true of two trees are exactly the same.
treeEqual :: Eq a => BTree a -> BTree a -> Bool
treeEqual Nil Nil = True
treeEqual _ Nil = False
treeEqual Nil _ = False
treeEqual (Node v1 left1 right1) (Node v2 left2 right2) = v1 == v2 && (treeEqual left1 left2) && (treeEqual right1 right2)


-- As homework, solve with treeTraverse + higher order function TO BE FANCY.
