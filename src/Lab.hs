--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive data types                                                  --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------
-- Red-black trees

data Colour = Red | Black
    deriving Eq

instance Show Colour where
    show Red = "Red"
    show Black = "Black"

data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving (Eq, Show)

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton n = Node Red Leaf n Leaf

makeBlack :: Tree a -> Tree a
makeBlack Leaf = Leaf
makeBlack (Node col left val right) = Node Black left val right

depth :: Tree a -> Int
depth Leaf = 0   
depth (Node _ left _ right) = max (depth left) (depth right) + 1

toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left val right) = toList left ++ [val] ++ toList right

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member n (Node _ left val right) = n == val || member n left || member n right

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = 
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = 
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = 
    Node Red (Node Black a x b) y (Node Black c z d)
balance c l x r = Node c l x r

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf n = Node Black Leaf n Leaf
insert (Node colour left val right) n
    | n > val = makeBlack (balance Black left val (insert' right n))
    | n < val = makeBlack (balance Black (insert' left n) val right)
    | otherwise = Node Black left val right

insert' :: Ord a => Tree a -> a -> Tree a
insert' Leaf n = singleton n
insert' (Node colour left val right) n
    | n > val = balance colour left val (insert' right n)
    | n < val = balance colour (insert' left n) val right
    | otherwise = Node colour left val right

--------------------------------------------------------------------------------
