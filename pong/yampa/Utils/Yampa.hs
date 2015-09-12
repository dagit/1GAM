{-# LANGUAGE Arrows #-}
module Utils.Yampa where

import Debug.Trace (trace)
import Control.Applicative
import Control.Monad (void, forever)
import Data.IORef
import FRP.Yampa
import GameState
import Utils.GLFW
import qualified Data.Time as T
import qualified Graphics.UI.GLFW as GLFW

traceSF :: SF String String
traceSF = proc s -> do
  arr (\s -> trace s s) -< s

traceShowSF :: Show a => SF a String
traceShowSF = arr show >>> traceSF

data External = KeyInput (GLFW.Key,Bool)
              | Graphics
              | Physics
              | NewBall
              | Resize (Int,Int)
  deriving (Show,Eq)

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

gameLoop :: GLFW.Window -> SF (Event External) (Event (IO Bool)) -> IO ()
gameLoop win sf = do
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
    GLFW.setFramebufferSizeCallback win $ Just $ \_win w h -> do
      resizeScene win w h
      void (react rh (0, Just (Event (Resize (w,h)))))
 
    -- TODO: This really shouldn't be necessary, but for some reason the event
    -- is not getting sent 
    (w,h) <- GLFW.getFramebufferSize win
    void (react rh (0, Just (Event (Resize (w,h))))) 

    -- register the keypress callback 
    GLFW.setKeyCallback win (Just $ \_win k _ s _ ->
      void (react rh (0, Just (Event (KeyInput (k,s==GLFW.KeyState'Pressed||s==GLFW.KeyState'Repeating))))))

    -- gameloop
    -- The timing code in the gameloop is from here: http://gafferongames.com/game-physics/fix-your-timestep/
    let dt = 0.01 -- The physics is simple so we use a very small dt for now and avoid the linear interpolation
    curTimeRef <- newIORef =<< getTime
    accRef     <- newIORef 0
    -- TODO: we don't currently pass elapsed time to the physics code
    -- tRef       <- newIORef 0
    forever $ do
      GLFW.pollEvents
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
      GLFW.swapBuffers win

isPressed :: ((GLFW.Key,Bool) -> Bool) -> SF (Event (GLFW.Key,Bool)) Bool
isPressed p = proc e -> do
  dHold False -< snd <$> filterE p e

isWPressed     :: SF (Event (GLFW.Key,Bool)) Bool
isWPressed     = isPressed (\(k,_) -> k == GLFW.Key'W)
isSPressed     :: SF (Event (GLFW.Key,Bool)) Bool
isSPressed     = isPressed (\(k,_) -> k == GLFW.Key'S)
isCommaPressed :: SF (Event (GLFW.Key,Bool)) Bool
isCommaPressed = isPressed (\(k,_) -> k == GLFW.Key'Comma)
isOPressed     :: SF (Event (GLFW.Key,Bool)) Bool
isOPressed     = isPressed (\(k,_) -> k == GLFW.Key'O)
isUpPressed    :: SF (Event (GLFW.Key,Bool)) Bool
isUpPressed    = isPressed (\(k,_) -> k == GLFW.Key'Up)
isDownPressed  :: SF (Event (GLFW.Key,Bool)) Bool
isDownPressed  = isPressed (\(k,_) -> k == GLFW.Key'Down)

handleQuit :: GLFW.Window -> a -> IO Bool
handleQuit win _  = shutdown win >> return True

collision :: (Float,Float) -> Float -> Collision
collision (min',max') a = case (a <= min', a >= max') of
  (False,False) -> None
  (True ,False) -> Min
  (False,True ) -> Max
  (True ,True ) -> Min -- This should never happen

-- | Conditional impulse
impulse :: a -> SF (a,Bool) a
impulse a = proc (p,bool) -> do
  returnA -< if bool then p else a

