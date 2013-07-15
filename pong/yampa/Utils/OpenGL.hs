{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Utils.OpenGL
( drawScene
, initGL
, drawNumber
, vSegment
, hSegment
, vHalfSegment
, zero
, one
, two
, three
, four
, five
, six
, seven
, eight
, nine
) where

import Control.Monad ( forM_ )
import Data.Bits ( (.|.) )
import GameState
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW as GLFW
import Utils.GLFW

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
-- Custom scene renderer
drawScene :: (Fractional a, Real a, Enum a) => (a,a) -> GameState -> IO ()
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
  rect (bX ball         , bY ball+ballDiam) -- top left
       (bX ball+ballDiam, bY ball+ballDiam) -- top right
       (bX ball+ballDiam, bY ball         ) -- bottom right
       (bX ball         , bY ball         ) -- bottom left

  -- Draw the paddles
  forM_ [rPaddle,lPaddle] $ \paddle -> do
    let (paddleX,paddleY) = pPos paddle

    rect (paddleX            ,paddleY+paddleHeight) -- top left
         (paddleX+paddleWidth,paddleY+paddleHeight) -- top right
         (paddleX+paddleWidth,paddleY             ) -- bottom right
         (paddleX            ,paddleY             ) -- bottom left

  -- Dotted line for the net
  forM_ [-w/2,-w/2+35..w/2] $ \d ->
    rect (-1,d+25) (1,d+25) (1,d) (-1,d)

  -- Draw the scores
  -- Left player score
  glTranslatef (- realToFrac w/4) (realToFrac h/2-40) 0
  drawNumber lScore
  -- Right player score
  glTranslatef (realToFrac w/2) 0 0
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

rect :: Real a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> IO ()
rect (tl1,tl2) (tr1,tr2) (br1,br2) (bl1,bl2) = do
  glBegin gl_QUADS
  glVertex2f (realToFrac tl1) (realToFrac tl2)
  glVertex2f (realToFrac tr1) (realToFrac tr2)
  glVertex2f (realToFrac br1) (realToFrac br2)
  glVertex2f (realToFrac bl1) (realToFrac bl2)
  glEnd

vSegment, hSegment :: IO ()
vSegment = rect (0,30) (2 ,30) (2 ,0) (0,0)
hSegment = rect (0,2)  (12,2)  (12,0) (0,0)

vHalfSegment :: IO ()
vHalfSegment = rect (0,15) (2,15) (2,0) (0,0)

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

