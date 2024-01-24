-- Functions that work on Paddles
module Paddle where

    
import Model


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
