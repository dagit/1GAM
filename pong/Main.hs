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

data Ball = Ball
  { ballX :: GLfloat
  , ballY :: GLfloat
  } deriving (Read, Show, Eq, Ord)

zeroBall :: Ball
zeroBall = Ball 0 0

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
  glOrtho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

drawScene :: Ball -> IO ()
drawScene ball = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  glBegin gl_QUADS
  glVertex2f (ballX ball)    (ballY ball+10) -- top left
  glVertex2f (ballX ball+10) (ballY ball+10) -- top right
  glVertex2f (ballX ball+10) (ballY ball)    -- bottom right
  glVertex2f (ballX ball)    (ballY ball)    -- bottom left
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
  gameLoop movingBall

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
 
type GameNetworkDescription = forall t. Event  t (GLFW.Key,Bool)       -- ^ user input
                                     -> Moment t (Behavior t (IO ())) -- ^ graphics to be sampled

-- | Useful for sequencing behaviors
-- eg., stepper a b &> stepper c d
(&>) :: (Applicative f, Applicative g) =>
     f (g a) -> f (g b) -> f (g b)
(&>) = liftA2 (*>)

movingBall :: GameNetworkDescription
movingBall input = do
  return (stepper (drawScene zeroBall) processInput)
  where
  processInput = unions
    [ drawScene  <$> move (fst <$> filterE snd input)
    -- We have to use a filterE on the event stream so that it only
    -- matches when the Esc key is actually pressed instead of
    -- always matching and doing nothing. In the later case it conflicts
    -- with the move event and then the order the events are combined
    -- determins which one actually fires (only one will fire, so either
    -- you can move the ball or you can quit, but not both).
    , handleQuit <$> filterE (\(k,b) -> k == GLFW.KeyEsc && b) input
    -- TODO: add more behaviors here
    ]

  moveBall :: (GLfloat,GLfloat) -> Ball -> Ball
  moveBall (x,y) b = b
    { ballX = ballX b + x
    , ballY = ballY b + y }

  moveBallInput :: GLFW.Key -> Ball -> Ball
  moveBallInput GLFW.KeyLeft  b = moveBall (-4,0) b
  moveBallInput GLFW.KeyRight b = moveBall ( 4,0) b
  moveBallInput GLFW.KeyUp    b = moveBall (0, 4) b
  moveBallInput GLFW.KeyDown  b = moveBall (0,-4) b
  moveBallInput _             b = b

  handleQuit  (GLFW.KeyEsc ,True)  = void shutdown
  handleQuit  _                    = return ()

  move :: Event t GLFW.Key -> Event t Ball
  move key = accumE zeroBall (moveBallInput <$> key)

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
    

