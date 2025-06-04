module Main where

import qualified TreeLib (testFunc, NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos, treePos2nodePos)
import Graphics.UI.GLUT
import Data.Int

type Point3D = (GLfloat, GLfloat, GLfloat)
type Points3D = [Point3D]

terrain_points = generateFlatTerrain 100 100 0

generateFlatTerrain :: Float -> Float -> Float -> Points3D
generateFlatTerrain width height depth =
    [(x, y, z) | x <- [-1, (-1 + 1/width)..1], y <- [-1, (-1 + 1 / height)..1], z <- [depth]]

-- TODO: need normalization here instead of /5.0
nodePos2Point3D :: TreeLib.NodePos -> Point3D
nodePos2Point3D (_, x , y) = (x/6.0 + 0.2, y/6.0 + 0.2 , 0.0)

nodePos2value :: TreeLib.NodePos -> Int
nodePos2value (value, _ , _) = value


generateFromNodePoses :: [TreeLib.NodePos] -> (Points3D, [Int])
generateFromNodePoses xs = ((map nodePos2Point3D xs), (map nodePos2value xs))

drawPoints :: Points3D -> IO ()
drawPoints points = renderPrimitive Points
                      $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points


display :: DisplayCallback
display = do
   -- clear all pixels
   clear [ ColorBuffer ]

   {-
   -- draw white polygon (rectangle) with corners at
   -- (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
   color (Color3 1.0 1.0 (1.0 :: GLfloat))
   -- resolve overloading, not needed in "real" programs
   let vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   renderPrimitive Polygon $ mapM_ vertex3f [
      Vertex3 0.25 0.25 0.0,
      Vertex3 0.75 0.25 0.0,
      Vertex3 0.75 0.75 0.0,
      Vertex3 0.25 0.75 0.0]
   -}
   color (Color3 0.0 0.0 (1.0 :: GLfloat))
   let tree = foldl TreeLib.bin_tree_insert TreeLib.Empty [2,5,17,1,4,3]
   let (_, tree_pos_list) = TreeLib.getTreePos tree 0.0 0.0
   let node_poses = map TreeLib.treePos2nodePos tree_pos_list
   let (tree_points, tree_values) = generateFromNodePoses node_poses
   -- drawPoints terrain_points
   drawPoints tree_points

   flush

myInit :: IO ()
myInit = do
   -- set clearing color
   clearColor $= Color4 0 0 0 0

   -- set viewing point
   matrixMode $= Projection
   loadIdentity
   ortho 0 1 0 1 (-1) 1

main :: IO ()
main = do
   _ <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 800 600
   initialWindowPosition $= Position 100 100
   _ <- createWindow "hello opengl"
   TreeLib.testFunc
   myInit
   displayCallback $= display
   mainLoop
