{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Arrows      #-}
module Pong
( pong
) where

import FRP.Yampa
import GameState
import Utils.OpenGL
import Utils.Yampa
import qualified Graphics.UI.GLFW as GLFW

initialState :: SF (Event External, (Float,Float)) (GameState, Event GameState)
initialState = proc (e, (w,h)) -> do
  let input     = filterKeyInput e
      spacebar  = filterE (\(k,b) -> k == GLFW.Key'Space && b) input
      startNow  = if isEvent spacebar then snd (fromEvent spacebar) else False
      rPaddle   = mkPaddle { pPos = rInitPos }
      lPaddle   = mkPaddle { pPos = lInitPos }
      rInitPos  = ( w / 2 - paddleWidth - 10, 0)
      lInitPos  = (-w / 2               + 10, 0)
      rPlayer   = mkPlayer { psPaddle = rPaddle { pPos = rInitPos } }
      lPlayer   = mkPlayer { psPaddle = lPaddle { pPos = lInitPos } }
      gameState = mkGameState { gsRPlayer  = rPlayer { psScore = 0 }
                              , gsLPlayer  = lPlayer { psScore = 0 }
                              , gsBall     = zeroBall }
  returnA -< (gameState, (e `tag` gameState) `gate` startNow)

doRound :: RandomGen g => g -> GameState -> SF (Event External,(Float,Float))
                                               GameState
doRound g initState = proc (a,(w,h)) -> do
  launch <- once -< a
  let input   = filterKeyInput a
      rPlayer = gsRPlayer initState
      lPlayer = gsLPlayer initState
      tick    = filterPhysics a
  initDirX <- (`reflect` 1) ^<< noise g -< launch
  initDirY <- (`reflect` 1) ^<< noise g -< launch
  initMagX <- noiseR (100,200) g -< launch
  initMagY <- noiseR (100,200) g -< launch
  -- w/s keys (, and o for dvorak) control the "Left" player's paddle
  lp <- leftPos  -< ((w,h), pSpeed (psPaddle lPlayer), input)
  -- up/down arrows control the "Right" player's paddle
  rp <- rightPos -< ((w,h), pSpeed (psPaddle rPlayer), input)
  let lPos = pPos (psPaddle (gsLPlayer initState)) ^+^ lp
      rPos = pPos (psPaddle (gsRPlayer initState)) ^+^ rp
      lPaddle = (psPaddle lPlayer) { pPos = lPos }
      rPaddle = (psPaddle rPlayer) { pPos = rPos }
  (bp,ce) <- ballPos -< (launch `tag` (initDirX*initMagX,initDirY*initMagY)
                        ,(lPaddle, rPaddle),(w,h),tick)
  rScore <- dAccumHold 0 -< filterE (== Min) ce `tag` (+1)
  lScore <- dAccumHold 0 -< filterE (== Max) ce `tag` (+1)
  let ball      = moveBall bp zeroBall
      gameState =
        mkGameState { gsRPlayer  = rPlayer { psScore  = rScore
                                           , psPaddle = rPaddle }
                    , gsLPlayer  = lPlayer { psScore  = lScore
                                           , psPaddle = lPaddle }
                    , gsBall     = ball }
  returnA -< gameState
  where
  reflect :: (Num a) => Bool -> a -> a
  reflect b v = if b then -v else v
  -- TODO: it would probably be better to separate the width/height
  -- from the tick event so that when the w/h changes there is one
  -- type of ballPos update (where we make sure it's in bounds)
  -- and a different type of update when it's a tick event
  -- where we calculate the position according to physics
  ballPos :: SF (Event (Float,Float),(Paddle,Paddle),(Float,Float),Event a) ((Float,Float),Event Collision)
  ballPos = proc (initV,(lPaddle,rPaddle),(w,h),e) -> do
    (iVx,iVy) <- hold (0,0) -< initV
    rec
      let ceilingFloorCollision = isCollision (collision (-h/2,h/2-ballDiam) y)
          wallCollisionTest     = collision (-w/2,w/2-ballDiam) x
          wallCollision         = isCollision wallCollisionTest
          collisionEvent        = e `tag` wallCollisionTest
          paddleCollisions      = paddleCollision lPaddle (x,y) || paddleCollision rPaddle (x+ballDiam,y)
      -- Correctly computing collisions here is sensitive to the velocity. If the
      -- velocity is high enough then the ball will actually travel into the thing it
      -- should be colliding with leading to hillarious results.
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
  rightPos = proc ((_,h),speed,e) -> do
    db <- isDownPressed -< e
    ub <- isUpPressed   -< e
    rec
      dv <- impulse (0,0) -< ((0,-speed), db && (-h/2 <= y))
      uv <- impulse (0,0) -< ((0, speed), ub && (y <= h/2 - paddleHeight))
      (x,y) <- integral   -< dv ^+^ uv
    returnA -< (x,y)

  -- | Controls the vertical position of the left paddle
  leftPos :: SF ((Float,Float),Float,Event (GLFW.Key, Bool)) (Float, Float)
  leftPos = proc ((_,h),speed,e) -> do
    wb <- isWPressed     -< e
    cb <- isCommaPressed -< e
    sb <- isSPressed     -< e
    ob <- isOPressed     -< e
    rec
      -- comma and o are support for dvorak users :-)
      dv <- impulse (0,0)  -< ((0,-speed), (sb || ob) && (-h/2 <= y))
      uv <- impulse (0,0)  -< ((0, speed), (wb || cb) && (y <= h/2 - paddleHeight))
      (x,y) <- integral    -< dv ^+^ uv
    returnA -< (x,y)

pong :: RandomGen g => GLFW.Window -> g -> SF (Event External) (Event (IO Bool))
pong win g = proc e -> do
  (w',h') <- hold (0,0) -< filterResize e
  let input     = filterKeyInput e
      graphics  = filterGraphics e
      (w,h)     = (fromIntegral w', fromIntegral h')
  gameState <- initialState `switch` doRound g -< (e, (w,h))
  returnA -< ((\_ -> drawScene (w,h) gameState >> return True) <$> graphics) `rMerge`
             (handleQuit win <$> filterE (\(k,b) -> k == GLFW.Key'Escape && b) input)
