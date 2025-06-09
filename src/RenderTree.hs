imodule RenderTree (Point3D, Points3D, Circles, TextPoints, generateFromNodePoses, normalizeEdges, drawPoints, drawCircle, drawEdges) where

import Graphics.UI.GLUT
import qualified TreeLib (NodePos, BTree (Empty, BBranch), bin_tree_insert, getTreePos, treePos2nodePos)
import Data.Int

type Point3D = (GLfloat, GLfloat, GLfloat)
type Points3D = [Point3D]
type Circles = [(Point3D, Float)]
type TextPoints = [(Point3D, String, Float)]

nodePos2Point3D :: Float -> Float -> TreeLib.NodePos -> Point3D
nodePos2Point3D max_x max_y (_, x , y) = ((x + 1.0)/(max_x + 2.0), (y + 1.0)/(max_y + 2.0) , 0.0)

normalizePoint3D :: Point3D -> Float -> Float -> Point3D
normalizePoint3D (x , y, z) max_x max_y = ((x + 1.0)/(max_x + 2.0), (y + 1.0)/(max_y + 2.0) , 0.0)

nodePos2value :: TreeLib.NodePos -> Int
nodePos2value (value, _ , _) = value

generateFromNodePoses :: [TreeLib.NodePos] -> Float -> Float -> (Points3D, [Int])
generateFromNodePoses xs max_x max_y = ((map (nodePos2Point3D max_x max_y) xs), (map nodePos2value xs))

normalizeEdges :: [(Point3D, Point3D)] -> Float -> Float -> [(Point3D, Point3D)]
normalizeEdges edges max_x max_y = map (\(p0, p1) -> ((normalizePoint3D p0 max_x max_y), (normalizePoint3D p1 max_x max_y))) edges

drawPoints :: Points3D -> IO ()
drawPoints points = renderPrimitive Points
                      $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

drawEdges :: [(Point3D, Point3D)] -> IO ()
drawEdges lines = drawLines lines
                  where drawLines liness = mapM_ drawLine liness
                        drawLine ((x1, y1, z1),(x2, y2, z2))= renderPrimitive Lines $ do
                                vertex $ (Vertex3 x1 y1 z1 :: Vertex3 GLfloat)
                                vertex $ (Vertex3 x2 y2 z2 :: Vertex3 GLfloat)

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
