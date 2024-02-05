module Model where


data Paddle = Paddle {
    paddle_x :: Float,
    paddle_y :: Float
} deriving (Show)


data Ball = Ball {
    ball_x :: Float,
    ball_y :: Float,
    ball_dx :: Float,
    ball_dy :: Float
} deriving (Show)


data Scores = Scores {
    score_1 :: Int,
    score_2 :: Int
} deriving (Show)


data GameState = Menu Float
               | Result Int 
               | Game {
    -- state :: State,
    paddles :: [Paddle],
    ball :: Ball,
    scores :: Scores,
    keypresses :: [KeyPress],
    wait :: Int,
    lastScorer :: Float
}
    deriving (Show)


data KeyPress = KeyS
              | KeyW
    deriving (Show, Eq)


initialBall :: Float -> Ball
initialBall dx = Ball {ball_x = 0.0, ball_y = 0.0, ball_dx = dx, ball_dy = 0}


initialPaddles :: [Paddle]
initialPaddles = [Paddle {paddle_x = -700.0, paddle_y = 0}, Paddle {paddle_x = 700.0, paddle_y = 0}]


initialState :: GameState
initialState = Game {
    -- state = Game,
    paddles = initialPaddles,
    ball = initialBall (-400.0),
    scores = Scores {score_1 = 0, score_2 = 0},
    keypresses = [],
    wait = 0,
    lastScorer = -1
}
