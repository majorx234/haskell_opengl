module Main where

import qualified TreeLib (testFunc, NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos, treePos2nodePos)
import Graphics.UI.GLUT
import Data.Int
import Debug.Trace

type Point3D = (GLfloat, GLfloat, GLfloat)
type Points3D = [Point3D]
type Circles = [(Point3D, Float)]
type TextPoints = [(Point3D, String, Float)]

terrain_points = generateFlatTerrain 100 100 0

generateFlatTerrain :: Float -> Float -> Float -> Points3D
generateFlatTerrain width height depth =
    [(x, y, z) | x <- [-1, (-1 + 1/width)..1], y <- [-1, (-1 + 1 / height)..1], z <- [depth]]

-- TODO: need normalization here instead of /5.0
nodePos2Point3D :: Float -> Float -> TreeLib.NodePos -> Point3D
nodePos2Point3D max_x max_y (_, x , y) = ((x + 1.0)/(max_x + 2.0), (y + 1.0)/(max_y + 2.0) , 0.0)

nodePos2value :: TreeLib.NodePos -> Int
nodePos2value (value, _ , _) = value


generateFromNodePoses :: [TreeLib.NodePos] -> Float -> Float -> (Points3D, [Int])
generateFromNodePoses xs max_x max_y = ((map (nodePos2Point3D max_x max_y) xs), (map nodePos2value xs))

drawPoints :: Points3D -> IO ()
drawPoints points = renderPrimitive Points
                      $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

calcCircle (x, y) radius divs = map toPoint angles where
    arc       = 2.0 * pi / fromIntegral divs
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..divs]

renderFan points = do
    renderPrimitive TriangleFan $ mapM_ (\(x, y) -> vertex (Vertex2 x y)) points

renderCircle (centre, radius, divs) = renderFan (centre : calcCircle centre radius divs)

-- renderText (centre, text, font_size) = 

drawCircle :: Circles -> IO ()
drawCircle circles_list = mapM_ renderCircle $ map (\((x, y, z), radius) -> ((x,y), radius, 64 )) circles_list

-- drawTextPoints :: TextPoints -> IO ()
-- drawTextPoints text_point_list = mapM_ renderText $ map (\((x, y, z), text, font_size -> ((x,y), text, font_size ))text_point_list

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
   let tree = foldl TreeLib.bin_tree_insert TreeLib.Empty [10,2,5,17,1,4,3]
   let (_, tree_pos_list) = TreeLib.getTreePos tree 0.0 0.0
   let node_poses = map TreeLib.treePos2nodePos tree_pos_list
   trace (show node_poses) $ pure ()
   let max_x = foldl max 0 (map (\(x, y, z) -> y) node_poses)
   let max_y = foldl max 0 (map (\(x, y, z) -> z) node_poses)
   let (tree_points, tree_values) = generateFromNodePoses node_poses max_x max_y
   trace (show tree_points) $ pure ()
   let num_tree_points = length tree_points
   -- drawPoints tree_points
   drawCircle $ zip tree_points $ replicate num_tree_points (0.1 / fromIntegral(num_tree_points))
   -- drawValues $ zip tree_points 

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
