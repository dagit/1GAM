module GameState where

data Ball = Ball
  { bX    :: Float
  , bY    :: Float
  , bVel  :: (Float,Float)
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
  { pSpeed  :: Float
  , pPos    :: (Float,Float)
  } deriving (Read, Show, Eq, Ord)

zeroBall :: Ball
zeroBall = Ball 0 0 (100,100)

ballDiam :: Float
ballDiam = 10

mkPaddle :: Paddle
mkPaddle = Paddle
  { pSpeed  = 1000
  , pPos    = (0,0)
  }

paddleWidth, paddleHeight :: Float
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

data Collision
  = None
  | Min
  | Max
  deriving (Read,Show,Eq,Ord)

isCollision :: Collision -> Bool
isCollision None = False
isCollision _    = True

moveBall :: (Float,Float) -> Ball -> Ball
moveBall (x,y) b = b
  { bX = bX b + x
  , bY = bY b + y }
