module View where


import Graphics.Gloss

import Model
import Draw
import Maths


renderPaddles :: [Paddle] -> Picture
renderPaddles ps = pictures $ map renderPaddle ps
    where renderPaddle p = translate (paddle_x p) (paddle_y p) $ color white $ rectangleSolid 15.0 80.0


renderBall :: Ball -> Picture
renderBall b = translate (ball_x b) (ball_y b) $ color white $ rectangleSolid 15.0 15.0


renderScores :: Scores -> Picture
renderScores scs = pictures
    [ color white $ rectangleSolid 2 900
    , translate (-40) 350 $ color white $ scale 2 2 $ drawNumber $ score_1 scs 
    , translate (40 + leftOffset (score_2 scs)) 350 $ color white $ scale 2 2 $ drawNumber $ score_2 scs]


render :: GameState -> IO Picture
render gs = do
    let pics = pictures [ renderPaddles (paddles gs)
                        , renderBall (ball gs)
                        , renderScores (scores gs)]

    return pics
