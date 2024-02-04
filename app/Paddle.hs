-- Functions that work on Paddles and Balls
module Paddle where

    
import Model

import GHC.Float


boundPaddle :: Paddle -> Paddle
boundPaddle p
    | paddle_y p >   360  = p { paddle_y =  360}
    | paddle_y p < (-360) = p { paddle_y = -360}
    | otherwise           = p


movePlayer :: Float -> [KeyPress] -> Paddle -> Paddle
movePlayer delta kp p1 
    | KeyS `elem` kp = p1 {paddle_y = paddle_y p1 + delta * (-350.0)}
    | KeyW `elem` kp = p1 {paddle_y = paddle_y p1 + delta *   350.0 }
    | otherwise      = p1


moveBall :: Float -> Ball -> Ball
moveBall delta b = b {
    ball_x = ball_x b + delta * ball_dx b,
    ball_y = ball_y b + delta * 40 * ball_dy b
}


bounceFromWall :: Ball -> Ball
bounceFromWall b
    | ball_y b >  392.5 = b { ball_y =  392.5, ball_dy = -ball_dy b}
    | ball_y b < -392.5 = b { ball_y = -392.5, ball_dy = -ball_dy b}
    | otherwise         = b


colliding :: Ball -> Paddle -> Bool
colliding b p = and 
    [ paddle_x p - 7.5 < ball_x b + 7.5
    , paddle_x p + 7.5 > ball_x b - 7.5
    , paddle_y p +  40 > ball_y b - 7.5
    , paddle_y p -  40 < ball_y b + 7.5 ]


bounceFactor :: Ball -> Paddle -> Int
bounceFactor b p = round $ (ball_y b - paddle_y p) / 4


placeNextTo :: Ball -> Paddle -> Float
placeNextTo b p | ball_x b > paddle_x p = paddle_x p + 30
                | otherwise             = paddle_x p - 30


bounceFromPaddle :: Float -> Ball -> Paddle -> Ball
bounceFromPaddle delta b p | colliding b p = move $ b {ball_x = placeNextTo b p, ball_dx = negate (ball_dx b), ball_dy = cap $ ball_dy b + int2Float (bounceFactor b p)}
                           | otherwise     = b
    where move b = b {ball_y = ball_y b + delta * 40 * ball_dy b}
          cap n | n < -5    = -5
                | n >  5    =  5
                | otherwise =  n
