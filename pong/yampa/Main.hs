module Main where

import Pong
import System.Random
import Utils.GLFW
import Utils.OpenGL
import Utils.Yampa
import qualified Graphics.UI.GLFW as GLFW

--------------------------------------------------------------------------

main :: IO ()
main = do
  True <- GLFW.init
  -- select type of display mode:
  -- Double buffer
  -- RGBA color
  -- Alpha components supported
  -- Depth buffer
  GLFW.defaultWindowHints
  -- open a window
  Just win <- GLFW.createWindow 800 600 "pong" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  initGL win
  -- window starts at upper left corner of the screen
  GLFW.setWindowCloseCallback win (Just shutdown)
  -- start event processing engine
  g <- newStdGen
  gameLoop win (pong win g)

