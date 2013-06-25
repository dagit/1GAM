{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Arrows      #-}
module Main where

import Control.Monad ( forever, void )
import Data.Bits ( (.|.) )
import Data.IORef
import FRP.Yampa hiding ((^+^))
import Control.Applicative
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import qualified Graphics.UI.GLFW as GLFW

import Data.VectorSpace
import qualified Data.Time as T

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
  glOrtho (-(fromIntegral (width `div` 2))) (fromIntegral (width `div` 2))
          (-(fromIntegral (height `div` 2))) (fromIntegral (height `div` 2)) (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

drawScene :: GameState -> IO ()
drawScene gs = do
  let ball                = gsBall gs
      rPaddle             = psPaddle (gsRPlayer gs)
      (rPaddleX,rPaddleY) = pPos rPaddle
      (rPaddleH,rPaddleW) = (pHeight rPaddle, pWidth rPaddle)
      lPaddle             = psPaddle (gsLPlayer gs)
      (lPaddleX,lPaddleY) = pPos lPaddle
      (lPaddleH,lPaddleW) = (pHeight lPaddle, pWidth lPaddle)
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  -- Draw the ball
  glBegin gl_QUADS
  glVertex2f (ballX ball)    (ballY ball+10) -- top left
  glVertex2f (ballX ball+10) (ballY ball+10) -- top right
  glVertex2f (ballX ball+10) (ballY ball)    -- bottom right
  glVertex2f (ballX ball)    (ballY ball)    -- bottom left
  glEnd

  -- Draw the right paddle
  glBegin gl_QUADS
  glVertex2f rPaddleX            (rPaddleY+rPaddleH) -- top left
  glVertex2f (rPaddleX+rPaddleW) (rPaddleY+rPaddleH) -- top right
  glVertex2f (rPaddleX+rPaddleW) rPaddleY            -- bottom right
  glVertex2f rPaddleX            rPaddleY            -- bottom left
  glEnd
  -- Draw the left paddle
  glBegin gl_QUADS
  glVertex2f lPaddleX            (lPaddleY+lPaddleH) -- top left
  glVertex2f (lPaddleX+lPaddleW) (lPaddleY+lPaddleH) -- top right
  glVertex2f (lPaddleX+lPaddleW) lPaddleY            -- bottom right
  glVertex2f lPaddleX            lPaddleY            -- bottom left
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

type GameNetworkDescription = SF (Event External)  -- ^ user input
                                 (Event (IO Bool)) -- ^ graphics to be sampled

data External = KeyInput (GLFW.Key,Bool)
              | Graphics
              | Physics

isKeyInput :: External -> Bool
isKeyInput (KeyInput{}) = True
isKeyInput _            = False

isGraphics :: External -> Bool
isGraphics Graphics = True
isGraphics _        = False

isPhysics :: External -> Bool
isPhysics Physics = True
isPhysics _       = False

fromKeyInput :: External -> (GLFW.Key,Bool)
fromKeyInput (KeyInput i) = i
fromKeyInput _            = error "fromKeyInput: called on non-KeyInput value"

filterPhysics  :: Event (External) -> Event ()
filterPhysics e = (const ()) <$> filterE isPhysics e

filterGraphics :: Event (External) -> Event ()
filterGraphics e = (const ()) <$> filterE isGraphics e

filterKeyInput :: Event (External) -> Event (GLFW.Key,Bool)
filterKeyInput e = fromKeyInput <$> filterE isKeyInput e

data GameState = GameState
  { gsLPlayer :: PlayerState -- ^ The "left" player
  , gsRPlayer :: PlayerState -- ^ The "right" player
  , gsBall    :: Ball        -- ^ The current state (position) of the ball
  } deriving (Read, Show, Eq, Ord)

data PlayerState = PlayerState
  { psPaddle :: Paddle -- ^ Position of the paddle and other details
  , psScore  :: Int    -- ^ The player's current score
  } deriving (Read, Show, Eq, Ord)

data Paddle = Paddle
  { pWidth  :: GLfloat
  , pHeight :: GLfloat
  , pSpeed  :: GLfloat
  , pPos    :: (GLfloat,GLfloat)
  } deriving (Read, Show, Eq, Ord)

mkPaddle :: Paddle
mkPaddle = Paddle
  { pWidth  = 10
  , pHeight = 100
  , pSpeed  = 100
  , pPos    = (0,0)
  }

mkPlayer :: PlayerState
mkPlayer = PlayerState
  { psPaddle = mkPaddle
  , psScore  = 0
  }

mkGameState :: GameState
mkGameState = GameState
  { gsLPlayer = mkPlayer
  , gsRPlayer = mkPlayer
  , gsBall    = zeroBall
  }

movingBall :: SF (Event External) (Event (IO Bool))
movingBall = proc e -> do
  let input    = filterKeyInput e
      graphics = filterGraphics e
      tick     = filterPhysics  e
  rp <- rightPos -< input
  lp <- leftPos  -< input
  -- up/down arrows control the "Right" player's paddle
  up <- upPos    -< input
  dp <- downPos  -< input
  -- The ball's position updates on each physics event
  bp <- ballPos  -< tick
  let rPlayer   = mkPlayer { psPaddle = mkPaddle { pPos = up ^+^ dp } }
      lPlayer   = mkPlayer { psPaddle = mkPaddle { pPos = rp ^+^ lp } }
      ball      = moveBall bp zeroBall
      gameState = mkGameState { gsRPlayer = rPlayer
                              , gsLPlayer = lPlayer
                              , gsBall    = ball }
  returnA -< ((\_ -> drawScene gameState >> return True) <$> graphics) `rMerge`
             (handleQuit <$> filterE (\(k,b) -> k == GLFW.KeyEsc && b) input)
  where
  moveBall :: (GLfloat,GLfloat) -> Ball -> Ball
  moveBall (x,y) b = b
    { ballX = ballX b + x
    , ballY = ballY b + y }

  handleQuit :: a -> IO Bool
  handleQuit  _  = shutdown

  pos :: (GLfloat,GLfloat) -> SF Bool (GLfloat,GLfloat)
  pos p = arr (\bool -> if bool then p else (0,0))

  ballPos :: SF (Event a) (GLfloat,GLfloat)
  ballPos = arr (\_ -> (100,100)) >>> integral

  -- rightPos and leftPos actually control the "Left" players
  -- paddle's vertical position. That's why the argument to
  -- pos might look funny.
  rightPos :: SF (Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  rightPos = isRightPressed >>> pos (0,100) >>> integral

  leftPos :: SF (Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  leftPos = isLeftPressed >>> pos (0,-100) >>> integral

  -- upPos and downPos control the vertical position of
  -- the "Right" player's paddle
  upPos :: SF (Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  upPos = isUpPressed >>> pos (0,100) >>> integral

  downPos :: SF (Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  downPos = isDownPressed >>> pos (0,-100) >>> integral

  isPressed :: ((GLFW.Key,Bool) -> Bool) -> SF (Event (GLFW.Key,Bool)) Bool
  isPressed p = proc e -> do
    accumHold False -< (\b _ -> snd b) <$> filterE p e

  isRightPressed :: SF (Event (GLFW.Key,Bool)) Bool
  isRightPressed = isPressed (\(k,_) -> k == GLFW.KeyRight)
  isLeftPressed  :: SF (Event (GLFW.Key,Bool)) Bool
  isLeftPressed  = isPressed (\(k,_) -> k == GLFW.KeyLeft)
  isUpPressed    :: SF (Event (GLFW.Key,Bool)) Bool
  isUpPressed    = isPressed (\(k,_) -> k == GLFW.KeyUp)
  isDownPressed  :: SF (Event (GLFW.Key,Bool)) Bool
  isDownPressed  = isPressed (\(k,_) -> k == GLFW.KeyDown)

gameLoop :: SF (Event External) (Event (IO Bool)) -> IO ()
gameLoop sf = do
    start <- T.getCurrentTime

    let getTime = fromRational . toRational . flip T.diffUTCTime start <$> T.getCurrentTime
        initialization = return NoEvent
        actuate _ _ NoEvent   = return False
        actuate _ _ (Event b) = b

    rh <- reactInit initialization actuate sf

    clock <- newIORef =<< getTime
    let sense ev = do
          t0 <- readIORef clock 
          t1 <- getTime

          let dt = t1 - t0
          writeIORef clock t1
          void (react rh (dt, Just (Event ev)))

    GLFW.setKeyCallback (\k b -> sense (KeyInput (k,b)))

    -- gameloop
    forever $ do
      GLFW.pollEvents
      sense Physics
      sense Graphics
      GLFW.swapBuffers
