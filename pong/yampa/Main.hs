{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Monad ( forever, void, forM_ )
import Data.Bits ( (.|.) )
import Data.IORef
import FRP.Yampa
import Foreign.C.Types (CFloat)
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import System.Random
import qualified Data.Time as T
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

drawScene :: (GLfloat,GLfloat) -> GameState -> IO ()
drawScene (w,_) gs = do
  let ball    = gsBall gs
      rPaddle = psPaddle (gsRPlayer gs)
      lPaddle = psPaddle (gsLPlayer gs)
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view

  -- Draw the ball
  glBegin gl_QUADS
  glVertex2f (bX ball)          (bY ball+ballDiam) -- top left
  glVertex2f (bX ball+ballDiam) (bY ball+ballDiam) -- top right
  glVertex2f (bX ball+ballDiam) (bY ball)          -- bottom right
  glVertex2f (bX ball)          (bY ball)          -- bottom left
  glEnd

  -- Draw the paddles
  forM_ [rPaddle,lPaddle] $ \paddle -> do
    let (paddleX,paddleY) = pPos paddle

    glBegin gl_QUADS
    glVertex2f paddleX               (paddleY+paddleHeight) -- top left
    glVertex2f (paddleX+paddleWidth) (paddleY+paddleHeight) -- top right
    glVertex2f (paddleX+paddleWidth) paddleY                -- bottom right
    glVertex2f paddleX               paddleY                -- bottom left
    glEnd

  -- Dotted line for the net
  forM_ [-w/2,-w/2+35..w/2] $ \d -> do
    glBegin gl_QUADS
    glVertex2f (-1) (d+25) -- top left
    glVertex2f   1  (d+25) -- top right
    glVertex2f   1  d      -- bottom right
    glVertex2f (-1) d      -- bottom left
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
  g <- newStdGen
  gameLoop (movingBall g)

type GameNetworkDescription = SF (Event External)  -- ^ user input
                                 (Event (IO Bool)) -- ^ graphics to be sampled

data External = KeyInput (GLFW.Key,Bool)
              | Graphics
              | Physics
              | NewBall
              | Resize (Int,Int)
  deriving (Read,Show,Eq,Ord)

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

isNewBall :: External -> Bool
isNewBall NewBall = True
isNewBall _       = False

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

filterNewBall :: Event (External) -> Event ()
filterNewBall e = const () <$> filterE isNewBall e

data Ball = Ball
  { bX    :: GLfloat
  , bY    :: GLfloat
  , bVel  :: (GLfloat,GLfloat)
  } deriving (Read, Show, Eq, Ord)

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
  { pSpeed  :: GLfloat
  , pPos    :: (GLfloat,GLfloat)
  } deriving (Read, Show, Eq, Ord)

zeroBall :: Ball
zeroBall = Ball 0 0 (100,100)

ballDiam :: GLfloat
ballDiam = 10

mkPaddle :: Paddle
mkPaddle = Paddle
  { pSpeed  = 1000
  , pPos    = (0,0)
  }

paddleWidth, paddleHeight :: GLfloat
paddleWidth  = 10
paddleHeight = 100

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

movingBall :: RandomGen g => g -> SF (Event External) (Event (IO Bool))
movingBall g = proc e -> do
  (w',h') <- hold (0,0) -< filterResize e
  let input    = filterKeyInput e
      graphics = filterGraphics e
      tick     = filterPhysics  e
      -- TODO: For now, a 'new ball' is served when space is pressed
      newBall  = const () <$> filterE (\(k,b) -> k == GLFW.CharKey ' ' && b) input
      (w,h)    = (fromIntegral w', fromIntegral h')
      rPaddle  = mkPaddle { pPos = rInitPos }
      lPaddle  = mkPaddle { pPos = lInitPos }
      rInitPos = ( w / 2 - paddleWidth - 10, 0)
      lInitPos = (-w / 2               + 10, 0)
  -- left/right arrows control the "Left" player's paddle
  lp <- leftPos  -< ((w,h), pSpeed rPaddle, input)
  -- up/down arrows control the "Right" player's paddle
  rp <- rightPos -< ((w,h), pSpeed lPaddle, input)
  -- The ball's position updates on each physics event
  let rPlayer = mkPlayer { psPaddle = rPaddle { pPos = rPos } }
      lPlayer = mkPlayer { psPaddle = lPaddle { pPos = lPos } }
      rPos    = rp ^+^ pPos rPaddle
      lPos    = lp ^+^ pPos lPaddle
  initDirX <- (\b -> if b then 1 else -1) ^<< noiseR (False,True) g'  -< newBall
  initDirY <- (\b -> if b then 1 else -1) ^<< noiseR (False,True) g'' -< newBall
  initMagX <- noiseR (100,200) g'''  -< newBall
  initMagY <- noiseR (100,200) g'''' -< newBall
  bp <- ballPos -< (newBall `tag` (initDirX*initMagX,initDirY*initMagY),(psPaddle lPlayer,psPaddle rPlayer),(w,h),tick)
  let ball      = moveBall bp zeroBall
      gameState = mkGameState { gsRPlayer = rPlayer
                              , gsLPlayer = lPlayer
                              , gsBall    = ball }
  returnA -< ((\_ -> drawScene (w,h) gameState >> return True) <$> graphics) `rMerge`
             (handleQuit <$> filterE (\(k,b) -> k == GLFW.KeyEsc && b) input)
  where
  (g',g'')     = split g
  (g''',g'''') = split g''
  moveBall :: (GLfloat,GLfloat) -> Ball -> Ball
  moveBall (x,y) b = b
    { bX = bX b + x
    , bY = bY b + y }

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
  ballPos :: SF (Event (GLfloat,GLfloat),(Paddle,Paddle),(GLfloat,GLfloat),Event a) (GLfloat,GLfloat)
  ballPos = proc (initV,(lPaddle,rPaddle),(w,h),e) -> mdo
    let ceilingFloorCollision = collision (-h/2,h/2-ballDiam) y
        wallCollision         = collision (-w/2,w/2-ballDiam) x
        paddleCollisions      = paddleCollision lPaddle (x,y) || paddleCollision rPaddle (x+ballDiam,y)
        reflect               = \b v -> if b then -v else v
    -- Correctly computing collisions here is sensitive to the velocity. If the
    -- velocity is high enough then the ball will actually travel into the thing it
    -- should be colliding with leading to hillarious results.
    (iVx,iVy) <- hold (0,0) -< initV
    vx <- accumHold 1 -< e `tag` reflect (wallCollision || paddleCollisions)
    vy <- accumHold 1 -< e `tag` reflect ceilingFloorCollision
    -- rSwitch :: SF a b -> SF (a, Event (SF a b)) b
    --(x,y) <- integral -< (vx*iVx,vy*iVy)
    (x,y) <- rSwitch integral -< ((vx*iVx,vy*iVy),initV `tag` integral)
    returnA -< (x,y)

  collision :: (GLfloat,GLfloat) -> GLfloat -> Bool
  collision (min',max') a = a <= min' || a >= max'

  paddleCollision :: Paddle -> (GLfloat,GLfloat) -> Bool
  paddleCollision p (x,y) = pX <= x && x <= pX + paddleWidth  &&
                            pY <= y && y <= pY + paddleHeight
    where
    (pX,pY) = pPos p

  -- | Controls the vertical position of the right paddle
  rightPos :: SF ((GLfloat,GLfloat),GLfloat, Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  rightPos = proc ((_,h),speed,e) -> mdo
    db <- isDownPressed -< e
    ub <- isUpPressed   -< e
    dv <- pos           -< ((0,-speed), db && (-h/2 <= y))
    uv <- pos           -< ((0, speed), ub && (y <= h/2 - paddleHeight))
    (x,y) <- integral   -< dv ^+^ uv
    returnA -< (x,y)

  -- | Controls the vertical position of the left paddle
  leftPos :: SF ((GLfloat,GLfloat),GLfloat,Event (GLFW.Key, Bool)) (GLfloat, GLfloat)
  leftPos = proc ((_,h),speed,e) -> mdo
    -- comma and o are support for dvorak users :-)
    wb <- isWPressed     -< e
    cb <- isCommaPressed -< e
    sb <- isSPressed     -< e
    ob <- isOPressed     -< e
    dv <- pos            -< ((0,-speed), (sb || ob) && (-h/2 <= y))
    uv <- pos            -< ((0, speed), (wb || cb) && (y <= h/2 - paddleHeight))
    (x,y) <- integral    -< dv ^+^ uv
    returnA -< (x,y)

  isPressed :: ((GLFW.Key,Bool) -> Bool) -> SF (Event (GLFW.Key,Bool)) Bool
  isPressed p = proc e -> do
    hold False -< snd <$> filterE p e

  isWPressed     :: SF (Event (GLFW.Key,Bool)) Bool
  isWPressed     = isPressed (\(k,_) -> k == GLFW.CharKey 'W')
  isSPressed     :: SF (Event (GLFW.Key,Bool)) Bool
  isSPressed     = isPressed (\(k,_) -> k == GLFW.CharKey 'S')
  isCommaPressed :: SF (Event (GLFW.Key,Bool)) Bool
  isCommaPressed = isPressed (\(k,_) -> k == GLFW.CharKey ',')
  isOPressed     :: SF (Event (GLFW.Key,Bool)) Bool
  isOPressed     = isPressed (\(k,_) -> k == GLFW.CharKey 'O')
  isUpPressed    :: SF (Event (GLFW.Key,Bool)) Bool
  isUpPressed    = isPressed (\(k,_) -> k == GLFW.KeyUp)
  isDownPressed  :: SF (Event (GLFW.Key,Bool)) Bool
  isDownPressed  = isPressed (\(k,_) -> k == GLFW.KeyDown)

{- No longer needed
  isRightPressed :: SF (Event (GLFW.Key,Bool)) Bool
  isRightPressed = isPressed (\(k,_) -> k == GLFW.KeyRight)
  isLeftPressed  :: SF (Event (GLFW.Key,Bool)) Bool
  isLeftPressed  = isPressed (\(k,_) -> k == GLFW.KeyLeft)
-}
gameLoop :: SF (Event External) (Event (IO Bool)) -> IO ()
gameLoop sf = do
    -- NOTE: This game loop probably violates an invariant of yampa
    -- by passing 0 for DTime in places
    start <- T.getCurrentTime

    let getTime = fromRational . toRational . flip T.diffUTCTime start <$> T.getCurrentTime
        initialization = return NoEvent
        actuate _ _ NoEvent   = return False
        actuate _ _ (Event b) = b
        while :: Monad m => m Bool -> m a -> m ()
        while mb ma = do
          b <- mb
          if b then ma >> while mb ma else return ()
    rh <- reactInit initialization actuate sf

    -- register the funciton called when our window is resized
    GLFW.setWindowSizeCallback (\w h -> resizeScene w h >> void (react rh (0, Just (Event (Resize (w,h))))))
   
    -- register the keypress callback 
    GLFW.setKeyCallback (\k b -> void (react rh (0, Just (Event (KeyInput (k,b))))))

    -- gameloop
    -- The timing code in the gameloop is from here: http://gafferongames.com/game-physics/fix-your-timestep/
    let dt = 0.01 -- The physics is simple so we use a very small dt for now and avoid the linear interpolation
    curTimeRef <- newIORef =<< getTime
    accRef     <- newIORef 0
    -- TODO: we don't currently pass elapsed time to the physics code
    -- tRef       <- newIORef 0
    forever $ do
      curTime <- readIORef curTimeRef
      newTime <- getTime
      let frameTime = min 0.25 (newTime - curTime)
      writeIORef  curTimeRef newTime
      modifyIORef accRef     (+ frameTime)
      while ((>= dt) <$> readIORef accRef) $ do
        -- t <- readIORef tRef
        void (react rh (dt, Just (Event Physics)))
        -- writeIORef  tRef   (t + dt)
        modifyIORef accRef (subtract dt)
      -- TODO: add linear interpolation step
      -- acc <- readIORef accRef
      -- let alpha = acc / dt
      -- putStrLn ("alpha = " ++ show alpha)
      -- void (react rh (alpha, Just (Event Lerp)))
      void (react rh (0, Just (Event Graphics)))
      GLFW.swapBuffers
