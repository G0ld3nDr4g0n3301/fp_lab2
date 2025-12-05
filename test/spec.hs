{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Test.Hspec
import Test.QuickCheck
import Bag
import Data.List (sort)

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary = fmap fromList arbitrary

main :: IO ()
main = hspec $ do
    
    describe "Bag Unit Tests" $ do
        
        it "empty bag is empty" $ do
            isEmpty empty `shouldBe` True

        it "insert adds an element" $ do
            let b = insert 5 empty
            toList b `shouldBe` [5]

        it "handles duplicates correctly (counts)" $ do
            let b = fromList [1, 1, 2]
            length b `shouldBe` 3
            toList b `shouldBe` [1, 1, 2]

        it "delete removes one instance of duplicate" $ do
            let b = fromList [5, 5]
            let b' = delete 5 b
            toList b' `shouldBe` [5]

        it "delete removes the node if count becomes 0" $ do
            let b = insert 5 empty
            let b' = delete 5 b
            isEmpty b' `shouldBe` True

        it "mapBag transforms elements" $ do
            let b = fromList [1, 2, 3]
            let b' = mapBag (*2) b
            toList b' `shouldBe` [2, 4, 6]

        it "filterBag removes unmatching elements and keeps counts" $ do
            let b = fromList [1, 2, 3, 4, 4, 5]
            let b' = filterBag even b
            toList b' `shouldBe` [2, 4, 4]

    describe "Bag Properties" $ do
        
        -- empty <> x == x
        it "Monoid Left Identity" $ property $ 
            \b -> (empty <> b) == (b :: Bag Int)

        -- x <> empty == x
        it "Monoid Right Identity" $ property $ 
            \b -> (b <> empty) == (b :: Bag Int)

        -- (a <> b) <> c == a <> (b <> c)
        it "Monoid Associativity" $ property $ 
            \a b c -> ((a <> b) <> c) == (a <> (b <> c :: Bag Int))

        -- Если вставить элемент, а потом удалить его, Bag не должен измениться
        it "Insert then Delete is Identity" $ property $
            \x b -> delete x (insert x b) == (b :: Bag Int)

        -- Размер объединения равен сумме размеров (bag size = length list)
        it "Size of union is sum of sizes" $ property $
            \a b -> length (a <> b) == length (a :: Bag Int) + length b

        -- Превращение в список всегда возвращает отсортированный список
        it "toList results in sorted list" $ property $
            \b -> let list = toList (b :: Bag Int)
                  in list == sort list

        -- Фильтрация возвращает подмножество исходного списка
        it "filterBag property (subset of toList, e.g., using even)" $ property $
            \b -> let p = even; list = toList (b :: Bag Int); filteredList = toList (filterBag p b)
                in all p filteredList && length filteredList <= length list