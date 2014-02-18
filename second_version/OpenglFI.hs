{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.Environment
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import ReadPoint
import FurthestInsertion 

getPoints args =
   if (length args == 0)
   then stdinReadPoints
   else readPoints (args!!0)  

minCoord sel p [] = sel p
minCoord sel p0 (p1:pts) 
   | sel p0 <= sel p1 = minCoord sel p0 pts
   | otherwise = minCoord sel p1 pts

maxCoord sel p [] = sel p
maxCoord sel p0 (p1:pts) 
   | sel p0 >= sel p1 = maxCoord sel p0 pts
   | otherwise = maxCoord sel p1 pts

type GLpoint = (GLdouble, GLdouble)
reshape :: [GLpoint] -> ReshapeCallback
reshape points size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   -- projects the tour into current window
   ortho2D ((minCoord fst (head points) points)-1) ((maxCoord fst (head points) points)+1)
           ((minCoord snd (head points) points)-1) ((maxCoord snd (head points) points)+1)
   matrixMode $= Modelview 0
   
-- Handles keyboard events: wainting for ESC key
keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()


main =
    getArgsAndInitialize >>=  \ (_, args) ->
    return  args >>=
    getPoints >>= 
    startFurthestInsertion >>=
    displayTour args 

title [] tour = "StdIn "++(show $ length tour)++" points"
title (filename:_) tour = filename++" "++(show $ length tour)++" points"

displayTour args tour = do
   initialDisplayMode $= [ RGBMode ]

   -- Create the window
   initialWindowSize $= Size 1024 768
   initialWindowPosition $= Position 100 150
   createWindow (title args tour)

   displayCallback $= display tour
   reshapeCallback $= Just (reshape tour)
   keyboardMouseCallback $= Just keyboard
   mainLoop

drawOneLine :: Vertex2 GLdouble -> Vertex2 GLdouble -> IO ()
drawOneLine p0 p1 = renderPrimitive Lines $ do vertex p0; vertex p1

display :: [(GLdouble,GLdouble)] -> DisplayCallback
display (points::[(GLdouble,GLdouble)]) = do --(points::[(GLdouble,GLdouble,GLdouble)]) = do
  color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
  clear [ColorBuffer]
  mapM_ (\ ((x0, y0), (x1, y1)) -> drawOneLine (Vertex2 x0 y0) (Vertex2 x1 y1)) (theTour points) 
  color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
  renderPrimitive Points $ mapM_ (\(x, y)->vertex$Vertex2 x y ) points
  flush

theTour :: [(GLdouble,GLdouble)] -> [((GLdouble, GLdouble), (GLdouble, GLdouble))]
theTour points = zip points (tail points ++ [head points])
