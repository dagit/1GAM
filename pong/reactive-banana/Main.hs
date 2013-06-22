{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad ( forever, void )
import Data.Bits ( (.|.) )
-- import Data.Time.Clock -- FIXME: time handling
import Graphics.Rendering.OpenGL.Raw
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import qualified Graphics.UI.GLFW as GLFW

import Data.VectorSpace
import Data.AffineSpace
import Data.Active (fromDuration, Time, toTime)
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

type GameNetworkDescription = forall t. Event    t (GLFW.Key,Bool)       -- ^ user input
                                     -> Behavior t Time
                                     -> Event    t ()
                                     -> Moment   t (Behavior t (IO ())) -- ^ graphics to be sampled

-- | Useful for sequencing behaviors
-- eg., stepper a b &> stepper c d
(&>) :: (Applicative f, Applicative g) =>
     f (g a) -> f (g b) -> f (g b)
(&>) = liftA2 (*>)

movingBall :: GameNetworkDescription
movingBall input time tick = do
  return $ (drawScene <$> (moveBall <$> (combinedPos (time <@ tick) input) <*> pure zeroBall)) &>
           (stepper (return ()) (handleQuit <$> filterE (\(k,b) -> k == GLFW.KeyEsc && b) input))

  where
  moveBall :: (GLfloat,GLfloat) -> Ball -> Ball
  moveBall (x,y) b = b
    { ballX = ballX b + x
    , ballY = ballY b + y }

  handleQuit :: (GLFW.Key,Bool) -> IO ()
  handleQuit  (GLFW.KeyEsc ,True)  = void shutdown
  handleQuit  _                    = return ()

  pos :: (GLfloat,GLfloat) -> Behavior t Bool -> Behavior t (GLfloat,GLfloat)
  pos p bBool = (\bool b -> if bool then p ^+^ b else b) <$> bBool <*> pure (0,0)

  rightPos :: Event t Time -> Event t (GLFW.Key,Bool) -> Behavior t (GLfloat,GLfloat)
  rightPos t k = integral t (pos (100,0) (isRightPressed k))

  leftPos :: Event t Time -> Event t (GLFW.Key,Bool) -> Behavior t (GLfloat,GLfloat)
  leftPos t k = integral t (pos (-100,0) (isLeftPressed k))

  upPos :: Event t Time -> Event t (GLFW.Key,Bool) -> Behavior t (GLfloat,GLfloat)
  upPos t k = integral t (pos (0,100) (isUpPressed k))

  downPos :: Event t Time -> Event t (GLFW.Key,Bool) -> Behavior t (GLfloat,GLfloat)
  downPos t k = integral t (pos (0,-100) (isDownPressed k))

  combinedPos t k = (^+^) <$> ((^+^) <$> rightPos t k  <*> leftPos t k)
                          <*> ((^+^) <$> upPos    t k  <*> downPos t k)

  isRightPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isRightPressed e = isPressed (\(k,_) -> k == GLFW.KeyRight) e

  isLeftPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isLeftPressed e = isPressed (\(k,_) -> k == GLFW.KeyLeft) e

  isUpPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isUpPressed e = isPressed (\(k,_) -> k == GLFW.KeyUp) e

  isDownPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isDownPressed e = isPressed (\(k,_) -> k == GLFW.KeyDown) e

  isPressed :: ((GLFW.Key,Bool) -> Bool) -> Event t (GLFW.Key,Bool) -> Behavior t Bool
  isPressed p e = accumB False ((\b _ -> snd b) <$> filterE p e)

gameLoop :: GameNetworkDescription -- ^ event network corresponding to the game
         -> IO ()
gameLoop gameNetwork = do
    -- set up event network
    start <- T.getCurrentTime
    let getTime = toTime . flip T.diffUTCTime start <$> T.getCurrentTime

    (ahKeyInput, fireKeyInput) <- newAddHandler
    (ahPhysics , firePhysics)  <- newAddHandler
    (ahGraphics, fireGraphics) <- newAddHandler

    network <- compile $ do
        eKeyInput <- fromAddHandler ahKeyInput
        ePhysics  <- fromAddHandler ahPhysics
        bTime     <- fromPoll getTime
        eGraphics <- fromAddHandler ahGraphics
        bGraphics <- gameNetwork eKeyInput bTime ePhysics
        reactimate $ bGraphics <@ eGraphics
    actuate network

    void (registerKeyHandler curry fireKeyInput)

    -- game loop
    forever $ do
      GLFW.pollEvents
      -- FIXME: set clock properly for user input

      -- "physics" simulation
      firePhysics ()           -- handle physics

      -- graphics
      fireGraphics ()          -- interpolate graphics

      GLFW.swapBuffers

-------------------------------------------------------------------------------

integral :: (VectorSpace v, Scalar v ~ GLfloat) => Event t Time -> Behavior t v -> Behavior t v
integral t b = sumB $ (\v dt -> fromDuration dt *^ v) <$> b <@> diffE t

sumB :: AdditiveGroup a => Event t a -> Behavior t a
sumB = accumB zeroV . fmap (^+^)

withPrevE :: Event t a -> Event t (a,a)
withPrevE = withPrevEWith (,)

withPrevEWith :: (a -> a -> b) -> Event t a -> Event t b
withPrevEWith f e = filterJust . fst . mapAccum Nothing $ g <$> e
    where
    g y Nothing  = (Nothing     , Just y)
    g y (Just x) = (Just (f y x), Just y)

diffE :: AffineSpace a => Event t a -> Event t (Diff a)
diffE = withPrevEWith (.-.)

unique :: Eq a => Event t a -> Event t a
unique = filterJust . accumE Nothing . fmap (\a acc -> if Just a == acc then Nothing else Just a)

once :: Event t a -> Event t a
once e = whenE (True `stepper` (False <$ e)) e

