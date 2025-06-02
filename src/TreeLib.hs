module TreeLib (testFunc) where

testFunc :: IO ()
testFunc = putStrLn "testFunc"

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show)


instance Eq a => Eq (Tree a) where
  Leaf a       == Leaf b             = a == b
  Branch l1 r1 == Branch l2 r2       = l1==l2 && r1==r2
  _ == _                             = False

instance Ord a => Ord (Tree a) where
  Leaf a < Leaf b             = a < b
  Leaf _ < Branch _ _         = True
  Branch _ _ < Leaf _         = False
  Branch l1 r1 < Branch l2 r2 = (l1 < l2) && (r1 < r2)
  t1           <= t2          = t1 < t2 || t1 == t2

  Leaf a > Leaf b             = a > b
  Leaf _ > Branch _ _         = True
  Branch _ _  > Leaf _        = False
  Branch l1 r1 > Branch l2 r2 = (l1 > l2) && (r1 > r2)
  t1           >= t2          = t1 > t2 || t1 == t2


data BTree = Empty | BBranch Int BTree BTree
  deriving (Eq, Show)

bin_tree_insert :: BTree -> Int -> BTree
bin_tree_insert Empty z = BBranch z Empty Empty
bin_tree_insert (BBranch x streel streer) z | z < x = (BBranch x (bin_tree_insert streel z) streer)
                                             | otherwise = BBranch x streel (bin_tree_insert  streer z)
