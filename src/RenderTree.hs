module RenderTree (Point3D, Points3D, Circles, TextPoints, generateFromNodePoses, drawPoints, drawCircle) where

import Graphics.UI.GLUT
import qualified TreeLib (testFunc, NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos, treePos2nodePos)
import Data.Int

type Point3D = (GLfloat, GLfloat, GLfloat)
type Points3D = [Point3D]
type Circles = [(Point3D, Float)]
type TextPoints = [(Point3D, String, Float)]

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
