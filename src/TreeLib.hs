module TreeLib (testFunc, BTree (Empty, BBranch), bin_tree_insert, getTreePos) where

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

type TreePos = (Float, Float, Float, Float, Int)

getTreePos :: BTree -> Float -> Float -> (TreePos, [TreePos])
getTreePos Empty layer x_offset = ((x_offset, layer, 0.0, 0.0, 0), [])
getTreePos (BBranch x Empty streer) layer x_offset  = let ((_, _, distrl, distrr, _), xsr) = getTreePos streer (layer + 1.0) (x_offset + 1.0)
                                                      in let current_pos = (x_offset, layer, 0.0, distrl + distrr + 1.0, x)
                                                         in (current_pos, current_pos: xsr)
getTreePos (BBranch x streel Empty) layer x_offset  = let ((_, _, distll, distlr, _), xsl) = getTreePos streel (layer + 1.0) (x_offset)
                                                      in let current_pos = (x_offset + distll + distlr + 1.0, layer, distll + distlr + 1.0, 0.0, x)
                                                         in (current_pos, current_pos: xsl)
getTreePos (BBranch x streel streer) layer x_offset  = let ((_, _, distll, distlr, _), xsl) = getTreePos streel (layer + 1.0) (x_offset)
                                                           in let dist_left = distll + distlr
                                                              in let ((_, _, distrl, distrr, _), xsr) = getTreePos streer (layer + 1.0) (x_offset + 1.0 + dist_left)
                                                                 in let dist_right = distrl + distrr + 1.0
                                                                    in let current_pos = (x_offset + dist_left, layer, dist_left, dist_right, x)
                                                                       in (current_pos, current_pos: (xsr ++ xsl))
