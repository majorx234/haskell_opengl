module Main where

import qualified TreeLib (testFunc)
import Graphics.UI.GLUT
import Data.Int

type Points3D = [(GLfloat, GLfloat, GLfloat)]

terrain_points = generateFlatTerrain 100 100 0

generateFlatTerrain :: Float -> Float -> Float -> Points3D
generateFlatTerrain width height depth =
    [(x, y, z) | x <- [-1, (-1 + 1/width)..1], y <- [-1, (-1 + 1 / height)..1], z <- [depth]]

drawPoints :: Points3D -> IO ()
drawPoints points = renderPrimitive Points
                      $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points


display :: DisplayCallback
display = do
   -- clear all pixels
   clear [ ColorBuffer ]

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
   color (Color3 0.0 0.0 (1.0 :: GLfloat))
   drawPoints terrain_points

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
