module Bag 
    (
    Bag
    , empty
    , isEmpty
    , insert
    , delete
    , fromList
    , mapBag
    , toList
    , filterBag
) where 

import Data.Foldable (toList)

data Bag a = Empty
    | Node a Int (Bag a) (Bag a)
    deriving (Show)

empty :: Bag a
empty = Empty

instance Foldable Bag where
    foldr _ z Empty = z
    foldr f z (Node val count left right) =
        foldr f fromVal left
        where
            fromRight = foldr f z right
            fromVal = foldr f fromRight (replicate count val)


--instance (Eq a) => Eq (Bag a) where
--    bagA == bagB = toList bagA == toList bagB

toDataList :: Bag a -> [(a, Int)]
toDataList Empty = []
toDataList (Node val count left right) =
    toDataList left ++ [(val, count)] ++ toDataList right

instance (Eq a) => Eq (Bag a) where
    bagA == bagB = toDataList bagA == toDataList bagB


isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _     = False

insert :: Ord a => a -> Bag a -> Bag a
insert x Empty = Node x 1 Empty Empty
insert x (Node val count left right)
    | x == val  = Node val (count + 1) left right
    | x < val   = Node val count (insert x left) right
    | otherwise   = Node val count left (insert x right)

delete :: (Ord a) => a -> Bag a -> Bag a
delete _ Empty = Empty
delete x (Node val count left right)
    | x < val   = Node val count (delete x left) right
    | x > val   = Node val count left (delete x right)
    | otherwise  =
        if count > 1
        then Node val (count - 1) left right
        else combine left right

combine :: Bag a -> Bag a -> Bag a 
combine Empty right = right
combine left Empty = left
combine left right =
    let ((minVal, minCount), newRight) = removeMin right
    in Node minVal minCount left newRight

removeMin :: Bag a -> ((a, Int), Bag a) 
removeMin Empty = error "removeMin is called on empty tree"
removeMin (Node val count Empty right) = ((val, count), right)
removeMin (Node val count left right) =
    let (minData,newLeft) = removeMin left
    in (minData, Node val count newLeft right)

fromList :: Ord a => [a] -> Bag a
fromList = foldr insert empty

instance Ord a => Semigroup (Bag a) where
    b1 <> b2 = foldr insert b2 b1

instance Ord a => Monoid (Bag a) where
    mempty = empty

mapBag :: (Ord b) => (a -> b) -> Bag a -> Bag b
mapBag f = foldr (insert . f) empty

filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag _ Empty = Empty
filterBag p (Node val count left right)
    | p val     = Node val count (filterBag p left) (filterBag p right)
    | otherwise = combine (filterBag p left) (filterBag p right)
