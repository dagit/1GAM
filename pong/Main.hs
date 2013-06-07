module Main where

import Control.Monad ( forever )
import Data.Bits ( (.|.) )
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import qualified Graphics.UI.GLFW as GLFW

initGL :: IO ()
initGL = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

--------------------------------------------------------------------------
-- GLFW callbacks
resizeScene :: GLFW.WindowSizeCallback
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  -- gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  gluOrtho2D 0 (fromIntegral width) (fromIntegral height) 0
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed k           b    = do
  putStrLn ("key " ++ show k ++ " is " ++ show b)
  hFlush stdout
  return ()

drawScene :: IO ()
drawScene = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  --glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0

  -- draw a triangle
  glBegin gl_TRIANGLES
  glVertex2f 0      1  -- top
  glVertex2f 1    (-1) -- bottom right
  glVertex2f (-1) (-1) -- bottom left
  glEnd


  -- glTranslatef 3 0 0  -- move right three units

  glBegin gl_QUADS
  glVertex2f 0  10 -- top left
  glVertex2f 10 10 -- top right
  glVertex2f 10 0 -- bottom right
  glVertex2f 0  0 -- bottom left
  glEnd

  glFlush
--------------------------------------------------------------------------


main :: IO ()
main = do
  True <- GLFW.initialize
  -- select type of display mode:
  -- Double buffer
  -- RGBA color
  -- Alpha components supported
  -- Depth buffer
  let dspOpts = GLFW.defaultDisplayOptions
                  -- get a 800 x 600 window
                  { GLFW.displayOptions_width  = 800
                  , GLFW.displayOptions_height = 600
                  -- Set depth buffering and RGBA colors
                  , GLFW.displayOptions_numRedBits   = 8
                  , GLFW.displayOptions_numGreenBits = 8
                  , GLFW.displayOptions_numBlueBits  = 8
                  , GLFW.displayOptions_numAlphaBits = 8
                  , GLFW.displayOptions_numDepthBits = 1
                  -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                  }
  -- open a window
  True <- GLFW.openWindow dspOpts
  -- window starts at upper left corner of the screen
  GLFW.setWindowPosition 0 0
  GLFW.setWindowTitle "pong"
  GLFW.setWindowRefreshCallback drawScene
  -- register the funciton called when our window is resized
  GLFW.setWindowSizeCallback resizeScene
  -- register the function called when the keyboard is pressed.
  GLFW.setKeyCallback keyPressed
  GLFW.setWindowCloseCallback shutdown
  -- initialize our window.
  -- start event processing engine
  forever $ do
    drawScene
    GLFW.swapBuffers


