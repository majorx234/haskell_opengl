module TreeLib (NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos,  treePos2nodePos) where

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
type NodePos = (Int, Float, Float)
type TreeEdge = ((Float, Float, Float),(Float, Float, Float))

getTreePos :: BTree -> Float -> Float -> (TreePos, [TreePos], [TreeEdge])
getTreePos Empty layer x_offset = ((x_offset, layer, 0.0, 0.0, 0), [], [])
getTreePos (BBranch x Empty streer) layer x_offset  = let ((xr, yr, distrl, distrr, _), xsr, edgesr) = getTreePos streer (layer + 1.0) (x_offset + 1.0)
                                                      in let current_pos = (x_offset, layer, 0.0, 0.0, x)
                                                         in (current_pos, current_pos: xsr, ((x_offset,layer,1.0), (xr,yr,1.0)): edgesr)
getTreePos (BBranch x streel Empty) layer x_offset  = let ((xl, yl, distll, distlr, _), xsl, edgesl) = getTreePos streel (layer + 1.0) (x_offset)
                                                      in let current_pos = (x_offset + distll + distlr + 1.0, layer, distll + distlr + 1.0, 0.0, x)
                                                         in (current_pos, current_pos: xsl, ((x_offset + distll + distlr + 1.0, layer,1.0), (xl,yl,1.0)): edgesl)
getTreePos (BBranch x streel streer) layer x_offset  = let ((xl, yl, distll, distlr, _), xsl, edgesl) = getTreePos streel (layer + 1.0) (x_offset)
                                                           in let dist_left = distll + 1.0 + distlr
                                                              in let ((xr, yr, distrl, distrr, _), xsr, edgesr) = getTreePos streer (layer + 1.0) (x_offset + 1.0 + dist_left)
                                                                 in let dist_right = distrl + distrr + 1.0
                                                                    in let current_pos = (x_offset + dist_left, layer, dist_left, dist_right, x)
                                                                       in (current_pos, current_pos: (xsr ++ xsl), ((x_offset + dist_left,layer,1.0), (xl,yl,1.0)): ((x_offset + dist_left,layer,1.0), (xr,yr,1.0)): (edgesr ++ edgesl))

treePos2nodePos :: TreePos -> (Int, Float, Float)
treePos2nodePos (x,y,_,_,value) = (value, x, y)
