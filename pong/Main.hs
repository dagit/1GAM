{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad ( forever, replicateM_, void )
import Data.Bits ( (.|.) )
import Data.IORef
-- import Data.Time.Clock -- FIXME: time handling
import Graphics.Rendering.OpenGL.Raw
import Reactive.Banana
import Reactive.Banana.Frameworks
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
  glOrtho 0 (fromIntegral width) (fromIntegral height) 0 (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

drawScene :: IO ()
drawScene = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  -- draw a triangle
  glBegin gl_TRIANGLES
  glVertex2f 0      1  -- top
  glVertex2f 1    (-1) -- bottom right
  glVertex2f (-1) (-1) -- bottom left
  glEnd

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
  hSetBuffering stdout NoBuffering
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
  -- register the funciton called when our window is resized
  GLFW.setWindowSizeCallback resizeScene
  GLFW.setWindowCloseCallback shutdown
  -- start event processing engine
  gameLoop emptyGame

---- copy & paste from stack overflow and other sources ----
-- http://stackoverflow.com/questions/12685430/how-to-implement-a-game-loop-in-reactive-banana
-- https://github.com/bernstein/breakout/blob/master/src/SdlAdapter.hs
-- https://gist.github.com/HeinrichApfelmus/3821030
--

-- TODO: this is just a stub and won't work correctly if you
-- need the time
getRealTime :: IO Integer
getRealTime = floor <$> GLFW.getTime

registerKeyHandler :: ((a -> IO ()) -> GLFW.KeyCallback) -> AddHandler a
registerKeyHandler handler sink = do
  putStrLn "called registerKeyHandler"
  GLFW.setKeyCallback (handler sink)
  return (return ())

-- timing helpers
fps, dt, ms :: Integer
fps  = 30                     -- physics framerate
dt   = (1000 `div` fps) * ms  -- physics timestep
ms   = 1000  -- TODO: what should this be?
 
type Duration = Integer
type Time     = Integer
 
type GameNetworkDescription = forall t. Event t (GLFW.Key,Bool)       -- ^ user input
                                     -> Moment t (Behavior t (IO ())) -- ^ graphics to be sampled

emptyGame :: GameNetworkDescription
emptyGame input = return (stepper drawScene (updateWorld <$> input))
  where
  updateWorld i = do
    handleKey i
    drawScene
  handleKey (GLFW.KeyEsc,True)  = void shutdown
  handleKey (i          ,True)  = do
    -- Go back to start of line
    putStr ("\r" ++ show i)
  handleKey (_          ,False) = do
    -- Hack to clear the previous line
    putStr "\r                    \r"

gameLoop :: GameNetworkDescription -- ^ event network corresponding to the game
         -> IO ()
gameLoop gameNetwork = do
    -- set up event network
    (ahKeyInput, fireKeyInput) <- newAddHandler
    (ahPhysics , firePhysics)  <- newAddHandler
    (ahGraphics, fireGraphics) <- newAddHandler
    clock <- newIORef 0
 
    network <- compile $ do
        eKeyInput <- fromAddHandler ahKeyInput
        ePhysics  <- fromAddHandler ahPhysics
        bTime     <- fromPoll (readIORef clock)
        eGraphics <- fromAddHandler ahGraphics
        bGraphics <- gameNetwork eKeyInput
        reactimate $ bGraphics <@ eGraphics
    actuate network

    void (registerKeyHandler curry fireKeyInput)

    -- This is a hack to get the network 'started'
    -- Note: This is no longer needed now that the
    -- initial value of the stepper is drawScene
    -- fireKeyInput (GLFW.KeyUnknown,False)

    let go clock acc old = do
        -- acc  accumulates excess time (usually < dt)
        -- old  keeps track of the time of the previous iteration of the game loop
        -- input <- SDL.pollEvent
        forever $ do
            new <- getRealTime
            GLFW.pollEvents
            -- FIXME: set clock properly for user input
 
            -- "physics" simulation
            -- invariant: the world time begins at 0 and is always a multiple of dt
            let (n,acc2) = (new - old + acc) `divMod` dt

            replicateM_ (fromIntegral n) $ do
                modifyIORef clock (+dt)  -- update clock
                firePhysics ()           -- handle physics

            -- no need to hog all the CPU
            -- FIXME: something with maximalFPS
            -- SDL.delay (dt `div` 3)
 
            -- graphics
            -- note: time might *not* be multiple of dt, for interpolation
            tempclock <- readIORef clock    -- remember multiple of dt
            modifyIORef clock (+acc2)       -- advance clock slightly
            fireGraphics ()                 -- interpolate graphics
            writeIORef  clock tempclock     -- reset clock to multiple of dt

            GLFW.swapBuffers
            go clock acc2 new

    -- game loop
    go clock 0 =<< getRealTime
    

