{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Arrows      #-}
module Pong
( pong
) where

import Control.Applicative
import FRP.Yampa
import GameState
import Utils.OpenGL
import Utils.Yampa
import qualified Graphics.UI.GLFW as GLFW

pong :: RandomGen g => g -> SF (Event External) (Event (IO Bool))
pong g = proc e -> mdo
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
  initDirX <- (`reflect` 1) ^<< noise g -< serveBall
  initDirY <- (`reflect` 1) ^<< noise g -< serveBall
  initMagX <- noiseR (100,200) g -< serveBall
  initMagY <- noiseR (100,200) g -< serveBall
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
  reflect :: (Num a) => Bool -> a -> a
  reflect b v = if b then -v else v
  -- TODO: it would probably be better to separate the width/height
  -- from the tick event so that when the w/h changes there is one
  -- type of ballPos update (where we make sure it's in bounds)
  -- and a different type of update when it's a tick event
  -- where we calculate the position according to physics
  ballPos :: SF (Event (Float,Float),(Paddle,Paddle),(Float,Float),Event a) ((Float,Float),Event Collision)
  ballPos = proc (initV,(lPaddle,rPaddle),(w,h),e) -> mdo
    let ceilingFloorCollision = isCollision (collision (-h/2,h/2-ballDiam) y)
        wallCollisionTest     = collision (-w/2,w/2-ballDiam) x
        wallCollision         = isCollision wallCollisionTest
        collisionEvent        = e `tag` wallCollisionTest
        paddleCollisions      = paddleCollision lPaddle (x,y) || paddleCollision rPaddle (x+ballDiam,y)
    -- Correctly computing collisions here is sensitive to the velocity. If the
    -- velocity is high enough then the ball will actually travel into the thing it
    -- should be colliding with leading to hillarious results.
    (iVx,iVy) <- hold (0,0) -< initV
    vx <- dAccumHold 1 -< e `tag` reflect (wallCollision || paddleCollisions)
    vy <- dAccumHold 1 -< e `tag` reflect ceilingFloorCollision
    (x,y) <- drSwitch integral -< ((vx*iVx,vy*iVy),initV `tag` integral)
    returnA -< ((x,y),filterE isCollision collisionEvent)

  paddleCollision :: Paddle -> (Float,Float) -> Bool
  paddleCollision p (x,y) = pX <= x && x <= pX + paddleWidth  &&
                            pY <= y && y <= pY + paddleHeight
    where
    (pX,pY) = pPos p

  -- | Controls the vertical position of the right paddle
  rightPos :: SF ((Float,Float),Float, Event (GLFW.Key, Bool)) (Float, Float)
  rightPos = proc ((_,h),speed,e) -> mdo
    db <- isDownPressed -< e
    ub <- isUpPressed   -< e
    dv <- impulse (0,0) -< ((0,-speed), db && (-h/2 <= y))
    uv <- impulse (0,0) -< ((0, speed), ub && (y <= h/2 - paddleHeight))
    (x,y) <- integral   -< dv ^+^ uv
    returnA -< (x,y)

  -- | Controls the vertical position of the left paddle
  leftPos :: SF ((Float,Float),Float,Event (GLFW.Key, Bool)) (Float, Float)
  leftPos = proc ((_,h),speed,e) -> mdo
    -- comma and o are support for dvorak users :-)
    wb <- isWPressed     -< e
    cb <- isCommaPressed -< e
    sb <- isSPressed     -< e
    ob <- isOPressed     -< e
    dv <- impulse (0,0)  -< ((0,-speed), (sb || ob) && (-h/2 <= y))
    uv <- impulse (0,0)  -< ((0, speed), (wb || cb) && (y <= h/2 - paddleHeight))
    (x,y) <- integral    -< dv ^+^ uv
    returnA -< (x,y)
