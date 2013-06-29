{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad ( forever, void )
import Data.Bits ( (.|.) )
import Data.IORef
import FRP.Yampa
import Control.Applicative
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import Foreign.C.Types (CFloat)
import System.IO
import qualified Graphics.UI.GLFW as GLFW

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
  GLFW.setWindowCloseCallback shutdown
  -- start event processing engine
  gameLoop movingBall

type GameNetworkDescription = SF (Event External)  -- ^ user input
                                 (Event (IO Bool)) -- ^ graphics to be sampled

data External = KeyInput (GLFW.Key,Bool)
              | Graphics
              | Physics
              | Resize (Int,Int)

isKeyInput :: External -> Bool
isKeyInput (KeyInput{}) = True
isKeyInput _            = False

isGraphics :: External -> Bool
isGraphics Graphics = True
isGraphics _        = False

isPhysics :: External -> Bool
isPhysics Physics = True
isPhysics _       = False

isResize :: External -> Bool
isResize (Resize {}) = True
isResize _           = False

fromKeyInput :: External -> (GLFW.Key,Bool)
fromKeyInput (KeyInput i) = i
fromKeyInput _            = error "fromKeyInput: called on non-KeyInput value"

fromResize :: External -> (Int,Int)
fromResize (Resize sz) = sz
fromResize _           = error "fromResize: called on non-Resize value"

filterPhysics  :: Event (External) -> Event ()
filterPhysics e = (const ()) <$> filterE isPhysics e

filterGraphics :: Event (External) -> Event ()
filterGraphics e = (const ()) <$> filterE isGraphics e

filterKeyInput :: Event (External) -> Event (GLFW.Key,Bool)
filterKeyInput e = fromKeyInput <$> filterE isKeyInput e

filterResize :: Event (External) -> Event (Int,Int)
filterResize e = fromResize <$> filterE isResize e

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
  , pSpeed  = 1000
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

instance VectorSpace CFloat CFloat where
    zeroVector = 0
    a *^ x = a * x
    x ^/ a = x / a
    negateVector x = (-x)
    x1 ^+^ x2 = x1 + x2
    x1 ^-^ x2 = x1 - x2
    x1 `dot` x2 = x1 * x2

movingBall :: SF (Event External) (Event (IO Bool))
movingBall = proc e -> do
  (w',h') <- accumHold (0,0) -< (\b _ -> b) <$> filterResize e
  let input    = filterKeyInput e
      graphics = filterGraphics e
      tick     = filterPhysics  e
      (w,h)    = (fromIntegral w', fromIntegral h')
      rPaddle  = mkPaddle { pPos = rInitPos }
      lPaddle  = mkPaddle { pPos = lInitPos }
      rInitPos = ( w / 2 - pWidth rPaddle - 10, 0)
      lInitPos = (-w / 2                  + 10, 0)
  -- left/right arrows control the "Left" player's paddle
  lp <- leftPos  -< ((w,h), pSpeed rPaddle, input)
  -- up/down arrows control the "Right" player's paddle
  rp <- rightPos -< ((w,h), pSpeed lPaddle, input)
  -- The ball's position updates on each physics event
  let rPlayer   = mkPlayer { psPaddle = rPaddle { pPos = rPos } }
      lPlayer   = mkPlayer { psPaddle = lPaddle { pPos = lPos } }
      rPos      = rp ^+^ pPos rPaddle
      lPos      = lp ^+^ pPos lPaddle
  bp <- ballPos -< ((psPaddle lPlayer,psPaddle rPlayer),(w,h),tick)
  let ball      = moveBall bp zeroBall
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

  pos :: SF ((GLfloat,GLfloat),Bool) (GLfloat,GLfloat)
  pos = proc (p,bool) -> do
    returnA -< if bool then p else (0,0)

  -- TODO: it would probably be better to separate the width/height
  -- from the tick event so that when the w/h changes there is one
  -- type of ballPos update (where we make sure it's in bounds)
  -- and a different type of update when it's a tick event
  -- where we calculate the position according to physics
  ballPos :: SF ((Paddle,Paddle),(GLfloat,GLfloat),Event a) (GLfloat,GLfloat)
  ballPos = proc ((lPaddle,rPaddle),(w,h),e) -> mdo
    -- TODO: this -10 is to adjust for the width of the ball
    let ceilingFloorCollision = collision (-h/2,h/2-10) y
        wallCollision         = collision (-w/2,w/2-10) x
        paddleCollisions      = paddleCollision lPaddle (x,y) || paddleCollision rPaddle (x+10,y)
        reflect               = \b v -> if b then -v else v
    -- Correctly computing collisions here is sensitive to the velocity. If the
    -- velocity is high enough then the ball will actually travel into the thing it
    -- should be colliding with leading to hillarious results.
    vx    <- accumHold 100 -< e `tag` reflect (wallCollision || paddleCollisions)
    vy    <- accumHold 100 -< e `tag` reflect ceilingFloorCollision
    (x,y) <- integral      -< (vx,vy)
    returnA -< (x,y)

  collision :: (GLfloat,GLfloat) -> GLfloat -> Bool
  collision (min',max') a = a <= min' || a >= max'

  paddleCollision :: Paddle -> (GLfloat,GLfloat) -> Bool
  paddleCollision p (x,y) = pX <= x && x <= pX + pWidth  p &&
                            pY <= y && y <= pY + pHeight p
    where
    (pX,pY) = pPos p

  -- rightPos and leftPos actually control the "Left" players
  -- paddle's vertical position. That's why the argument to
  -- pos might look funny.
  rightPos :: SF ((GLfloat,GLfloat),GLfloat, Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  rightPos = proc ((_,h),speed,e) -> mdo
    db <- isDownPressed -< e
    ub <- isUpPressed   -< e
    dv <- pos           -< ((0,-speed), db && (-h/2 <= y))
    -- TODO: pass in paddle dimensions
    uv <- pos           -< ((0, speed), ub && (y <= h/2 - 100))
    (x,y) <- integral   -< dv ^+^ uv
    returnA -< (x,y)

  leftPos :: SF ((GLfloat,GLfloat),GLfloat,Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  leftPos = proc ((_,h),speed,e) -> mdo
    lb <- isLeftPressed  -< e
    rb <- isRightPressed -< e
    dv <- pos            -< ((0,-speed), lb && (-h/2 <= y))
    -- TODO: pass in paddle dimensions
    uv <- pos            -< ((0, speed), rb && (y <= h/2 - 100))
    (x,y) <- integral    -< dv ^+^ uv
    returnA -< (x,y)

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

    -- register the funciton called when our window is resized
    GLFW.setWindowSizeCallback  (\w h -> resizeScene w h >> sense (Resize (w,h)))
    
    GLFW.setKeyCallback (\k b -> sense (KeyInput (k,b)))

    -- gameloop
    forever $ do
      GLFW.pollEvents
      sense Physics
      sense Graphics
      GLFW.swapBuffers
