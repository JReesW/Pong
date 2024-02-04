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


data GameState = GameState {
    paddles :: [Paddle],
    ball :: Ball,
    scores :: Scores,
    keypresses :: [KeyPress]
} deriving (Show)


data KeyPress = KeyS
              | KeyW
    deriving (Show, Eq)


initialState :: GameState
initialState = GameState {
    paddles = [Paddle {paddle_x = -700.0, paddle_y = 0}, Paddle {paddle_x = 700.0, paddle_y = 0}],
    ball = Ball {ball_x = -150.0, ball_y = 0.0, ball_dx = -400.0, ball_dy = 0},
    scores = Scores {score_1 = 0, score_2 = 0},
    keypresses = []
}
