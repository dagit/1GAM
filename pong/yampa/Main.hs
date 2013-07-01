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
drawScene (w,h) gs = do
  let ball    = gsBall gs
      rPaddle = psPaddle (gsRPlayer gs)
      lPaddle = psPaddle (gsLPlayer gs)
      lScore  = psScore (gsLPlayer gs)
      rScore  = psScore (gsRPlayer gs)
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


  -- Draw the scores
  -- Left player score
  glTranslatef (-w/4) (h/2-40) 0
  drawNumber lScore
  -- Right player score
  glTranslatef (w/2) 0 0
  drawNumber rScore

  glFlush

drawNumber :: Int -> IO ()
drawNumber n = let ds = map (\x -> read [x]) (show n) :: [Int]
  in mapM_ (\d -> drawDigit d >> glTranslatef 20 0 0) ds
  where
  drawDigit :: Int -> IO ()
  drawDigit 0 = zero
  drawDigit 1 = one
  drawDigit 2 = two
  drawDigit 3 = three
  drawDigit 4 = four
  drawDigit 5 = five
  drawDigit 6 = six
  drawDigit 7 = seven
  drawDigit 8 = eight
  drawDigit 9 = nine
  drawDigit _ = return ()

rect :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> IO ()
rect (tl1,tl2) (tr1,tr2) (br1,br2) (bl1,bl2) = do
  glBegin gl_QUADS
  glVertex2f tl1 tl2
  glVertex2f tr1 tr2
  glVertex2f br1 br2
  glVertex2f bl1 bl2
  glEnd

vSegment, hSegment :: IO ()
vSegment = rect (0,30) (2 ,30) (2 ,0) (0,0)
hSegment = rect (0,2)  (12,2)  (12,0) (0,0)

vHalfSegment, hHalfSegment :: IO ()
vHalfSegment = rect (0,15) (2,15) (2,0) (0,0)
hHalfSegment = rect (0,2)  (6,2)  (6,0) (0,0)

zero :: IO ()
zero = do
  vSegment
  glTranslatef 12 0 0
  vSegment
  glTranslatef (-12) 0 0
  hSegment
  glTranslatef 0 28 0
  hSegment
  glTranslatef 0 (-28) 0

one :: IO ()
one = do
  glTranslatef 12 0 0
  vSegment
  glTranslatef (-12) 0 0

two :: IO ()
two = do
  hSegment
  vHalfSegment
  glTranslatef 0 15 0
  hSegment
  glTranslatef 12 0 0
  vHalfSegment
  glTranslatef (-12) 13 0
  hSegment
  glTranslatef 0 (-28) 0

three :: IO ()
three = do
  hSegment
  glTranslatef 0 15 0
  hSegment
  glTranslatef 0 13 0
  hSegment
  glTranslatef 12 (-28) 0
  vSegment
  glTranslatef (-12) 0 0

four :: IO ()
four = do
  glTranslatef 12 0 0
  vSegment
  glTranslatef (-12) 15 0
  hSegment
  vHalfSegment
  glTranslatef (12) (-2) 0
  vHalfSegment
  glTranslatef (-12) (-13) 0

five :: IO ()
five = do
  hSegment
  glTranslatef 12 0 0
  vHalfSegment
  glTranslatef (-12) 13 0
  hSegment
  vHalfSegment
  glTranslatef 0 15 0
  hSegment
  glTranslatef 0 (-28) 0

six :: IO ()
six = do
  hSegment
  vSegment
  glTranslatef 10 0 0
  vHalfSegment
  glTranslatef (-10) 15 0
  hSegment
  glTranslatef 0 13 0
  hSegment
  glTranslatef 0 (-28) 0

seven :: IO ()
seven = do
  glTranslatef 12 0 0
  vSegment
  glTranslatef (-12) 28 0
  hSegment
  glTranslatef 0 (-28) 0

eight :: IO ()
eight = do
  hSegment
  vSegment
  glTranslatef 0 15 0
  hSegment
  glTranslatef 0 13 0
  hSegment
  glTranslatef 12 (-28) 0
  vSegment
  glTranslatef (-12) 0 0

nine :: IO ()
nine = do
  glTranslatef 12 0 0
  vSegment
  glTranslatef (-12) 15 0
  hSegment
  vHalfSegment
  glTranslatef 0 13 0
  hSegment
  glTranslatef 0 (-28) 0
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

data Collision
  = None
  | Min
  | Max
  deriving (Read,Show,Eq,Ord)

isCollision :: Collision -> Bool
isCollision None = False
isCollision _    = True

movingBall :: RandomGen g => g -> SF (Event External) (Event (IO Bool))
movingBall g = proc e -> mdo
  (w',h') <- dHold (0,0) -< filterResize e
  let input     = filterKeyInput e
      graphics  = filterGraphics e
      tick      = filterPhysics  e
      spacebar  = () <$ filterE (\(k,b) -> (k == GLFW.CharKey ' ' || k == GLFW.KeySpace) && b) input
      -- TODO: For now, a 'new ball' is served when space is pressed
      serveBall = spacebar `gate` canServe
      (w,h)     = (fromIntegral w', fromIntegral h')
      rPaddle   = mkPaddle { pPos = rInitPos }
      lPaddle   = mkPaddle { pPos = lInitPos }
      rInitPos  = ( w / 2 - paddleWidth - 10, 0)
      lInitPos  = (-w / 2               + 10, 0)
  canServe <- dHold True -< (serveBall `tag` False) `rMerge` (ce `tag` True)
  -- w/s keys (, and o for dvorak) control the "Left" player's paddle
  lp <- leftPos  -< ((w,h), pSpeed rPaddle, input)
  -- up/down arrows control the "Right" player's paddle
  rp <- rightPos -< ((w,h), pSpeed lPaddle, input)
  -- The ball's position updates on each physics event
  let rPlayer = mkPlayer { psPaddle = rPaddle { pPos = rPos } }
      lPlayer = mkPlayer { psPaddle = lPaddle { pPos = lPos } }
      rPos    = rp ^+^ pPos rPaddle
      lPos    = lp ^+^ pPos lPaddle
  initDirX <- (\b -> if b then 1 else -1) ^<< noiseR (False,True) g'  -< serveBall
  initDirY <- (\b -> if b then 1 else -1) ^<< noiseR (False,True) g'' -< serveBall
  initMagX <- noiseR (100,200) g'''  -< serveBall
  initMagY <- noiseR (100,200) g'''' -< serveBall
  -- Note: This switch is brittle. For example, if you change to rSwitch then you get
  -- a cycle and ghc prints <<loop>>
  (bp,ce)  <- drSwitch ballPos -< ((serveBall `tag` (initDirX*initMagX,initDirY*initMagY)
                                   ,(psPaddle lPlayer,psPaddle rPlayer),(w,h),tick)
                                  ,ce `tag` ballPos)
  rScore <- dAccumHold 0 -< filterE (== Min) ce `tag` (+1)
  lScore <- dAccumHold 0 -< filterE (== Max) ce `tag` (+1)
  let ball      = moveBall bp zeroBall
      gameState = mkGameState { gsRPlayer = rPlayer { psScore = rScore }
                              , gsLPlayer = lPlayer { psScore = lScore }
                              , gsBall    = ball }
  returnA -< ((\_ -> drawScene (w,h) gameState >> return True) <$> graphics) `rMerge`
             (handleQuit <$> filterE (\(k,b) -> k == GLFW.KeyEsc && b) input)
  where
  -- Ugh. We have to manually split the g to avoid
  -- using the same random seed everywhere :(
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
  ballPos :: SF (Event (GLfloat,GLfloat),(Paddle,Paddle),(GLfloat,GLfloat),Event a) ((GLfloat,GLfloat),Event Collision)
  ballPos = proc (initV,(lPaddle,rPaddle),(w,h),e) -> mdo
    let ceilingFloorCollision = isCollision (collision (-h/2,h/2-ballDiam) y)
        wallCollisionTest     = collision (-w/2,w/2-ballDiam) x
        wallCollision         = isCollision wallCollisionTest
        collisionEvent        = e `tag` wallCollisionTest
        paddleCollisions      = paddleCollision lPaddle (x,y) || paddleCollision rPaddle (x+ballDiam,y)
        reflect               = \b v -> if b then -v else v
    -- Correctly computing collisions here is sensitive to the velocity. If the
    -- velocity is high enough then the ball will actually travel into the thing it
    -- should be colliding with leading to hillarious results.
    (iVx,iVy) <- hold (0,0) -< initV
    vx <- dAccumHold 1 -< e `tag` reflect (wallCollision || paddleCollisions)
    vy <- dAccumHold 1 -< e `tag` reflect ceilingFloorCollision
    (x,y) <- drSwitch integral -< ((vx*iVx,vy*iVy),initV `tag` integral)
    returnA -< ((x,y),filterE isCollision collisionEvent)

  collision :: (GLfloat,GLfloat) -> GLfloat -> Collision
  collision (min',max') a = case (a <= min', a >= max') of
    (False,False) -> None
    (True ,False) -> Min
    (False,True ) -> Max
    (True ,True ) -> Min -- This should never happen

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
    dHold False -< snd <$> filterE p e

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
