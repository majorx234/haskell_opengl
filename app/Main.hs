module Main where

import qualified TreeLib (NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos, treePos2nodePos)
import qualified RenderTree (generateFromNodePoses,  normalizeEdges, drawCircle, drawEdges, drawText, drawText2)
import Graphics.UI.GLUT
import Data.Int
import Debug.Trace

display :: DisplayCallback
display = do
   -- clear all pixels
   clear [ ColorBuffer ]

   color (Color3 0.0 0.0 (1.0 :: GLfloat))
   let tree = foldl TreeLib.bin_tree_insert TreeLib.Empty [10,2,5,17,1,4,3,8,9]
   let (_, tree_pos_list, tree_edges) = TreeLib.getTreePos tree 0.0 0.0
   let node_poses = map TreeLib.treePos2nodePos tree_pos_list
   let max_x = foldl max 0 (map (\(x, y, z) -> y) node_poses)
   let max_y = foldl max 0 (map (\(x, y, z) -> z) node_poses)
   let (tree_points, tree_values) = RenderTree.generateFromNodePoses node_poses max_x max_y
   let num_tree_points = length tree_points
   let edges = RenderTree.normalizeEdges tree_edges max_x max_y
   RenderTree.drawEdges edges
   RenderTree.drawCircle $ zip tree_points $ replicate num_tree_points (0.1 / fromIntegral(num_tree_points))
   RenderTree.drawText2 $ zip3 tree_points (map show tree_values) $ replicate num_tree_points (0.1 / fromIntegral(num_tree_points))


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
   _ <- createWindow "treeviz"
   myInit
   displayCallback $= display
   mainLoop
