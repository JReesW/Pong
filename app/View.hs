module View where


import Graphics.Gloss

import Model


renderPaddles :: [Paddle] -> Picture
renderPaddles ps = pictures $ map renderPaddle ps
    where renderPaddle p = translate (paddle_x p) (paddle_y p) $ color white $ rectangleSolid 15.0 80.0


renderBall :: Ball -> Picture
renderBall b = translate (ball_x b) (ball_y b) $ color white $ rectangleSolid 10.0 10.0


renderScores :: Scores -> Picture
renderScores scs = circle 1


render :: GameState -> Picture
render gs = pictures 
    [ renderPaddles (paddles gs)
    , renderBall (ball gs)
    , renderScores (scores gs)]
