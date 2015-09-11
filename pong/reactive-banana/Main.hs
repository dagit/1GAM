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

initGL :: GLFW.Window -> IO ()
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

--------------------------------------------------------------------------
-- GLFW callbacks
resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  glOrtho (-(fromIntegral (width `div` 2))) (fromIntegral (width `div` 2))
          (-(fromIntegral (height `div` 2))) (fromIntegral (height `div` 2)) (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

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
  -- register the funciton called when our window is resized
  GLFW.setWindowSizeCallback  win (Just resizeScene)
  GLFW.setWindowCloseCallback win (Just shutdown)
  -- start event processing engine
  gameLoop win movingBall

---- copy & paste from stack overflow and other sources ----
-- http://stackoverflow.com/questions/12685430/how-to-implement-a-game-loop-in-reactive-banana
-- https://github.com/bernstein/breakout/blob/master/src/SdlAdapter.hs
-- https://gist.github.com/HeinrichApfelmus/3821030
--

-- TODO: this is just a stub and won't work correctly if you
-- need the time
getRealTime :: IO Integer
getRealTime = floor <$> do
  x <- GLFW.getTime
  return $ case x of
   Nothing -> 0
   Just y  -> y

registerKeyHandler :: GLFW.Window -> (Handler a -> GLFW.KeyCallback) -> Handler a -> IO ()
registerKeyHandler win handler sink = do
  putStrLn "called registerKeyHandler"
  GLFW.setKeyCallback win (Just (handler sink))
  return ()

type GameNetworkDescription = forall t. GLFW.Window
                                     -> Event    t (GLFW.Key,Bool)       -- ^ user input
                                     -> Behavior t Time
                                     -> Event    t ()
                                     -> Moment   t (Behavior t (IO ())) -- ^ graphics to be sampled

-- | Useful for sequencing behaviors
-- eg., stepper a b &> stepper c d
(&>) :: (Applicative f, Applicative g) =>
     f (g a) -> f (g b) -> f (g b)
(&>) = liftA2 (*>)

movingBall :: GameNetworkDescription
movingBall win input time tick = do
  return $ (drawScene <$> (moveBall <$> (combinedPos (time <@ tick) input) <*> pure zeroBall)) &>
           (stepper (return ()) (handleQuit win <$> filterE (\(k,b) -> k == GLFW.Key'Escape && b) input))

  where
  moveBall :: (GLfloat,GLfloat) -> Ball -> Ball
  moveBall (x,y) b = b
    { ballX = ballX b + x
    , ballY = ballY b + y }

  handleQuit :: GLFW.Window -> (GLFW.Key,Bool) -> IO ()
  handleQuit  w (GLFW.Key'Escape ,True)  = void (shutdown w)
  handleQuit  _ _                        = return ()

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
  isRightPressed e = isPressed (\(k,_) -> k == GLFW.Key'Right) e

  isLeftPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isLeftPressed e = isPressed (\(k,_) -> k == GLFW.Key'Left) e

  isUpPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isUpPressed e = isPressed (\(k,_) -> k == GLFW.Key'Up) e

  isDownPressed :: Event t (GLFW.Key,Bool) -> Behavior t Bool
  isDownPressed e = isPressed (\(k,_) -> k == GLFW.Key'Down) e

  isPressed :: ((GLFW.Key,Bool) -> Bool) -> Event t (GLFW.Key,Bool) -> Behavior t Bool
  isPressed p e = accumB False ((\b _ -> snd b) <$> filterE p e)

gameLoop :: GLFW.Window
         -> GameNetworkDescription -- ^ event network corresponding to the game
         -> IO ()
gameLoop win gameNetwork = do
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
        bGraphics <- gameNetwork win eKeyInput bTime ePhysics
        reactimate $ bGraphics <@ eGraphics
    actuate network

--    void (registerKeyHandler uncurry fireKeyInput)
    void (registerKeyHandler win
                             (\f _win k _i s _m -> do
                               f (k,s==GLFW.KeyState'Pressed || s==GLFW.KeyState'Repeating)
                               return ())
                             fireKeyInput)

    -- game loop
    forever $ do
      GLFW.pollEvents
      -- FIXME: set clock properly for user input

      -- "physics" simulation
      firePhysics ()           -- handle physics

      -- graphics
      fireGraphics ()          -- interpolate graphics

      GLFW.swapBuffers win

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

