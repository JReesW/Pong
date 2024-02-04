module Draw where


import Data.Bits
import Graphics.Gloss


import Maths


drawSegment :: Int -> Picture
drawSegment 0 = translate     0     15  $ rectangleSolid 20 5
drawSegment 1 = translate   7.5    7.5  $ rectangleSolid 5 20
drawSegment 2 = translate   7.5  (-7.5) $ rectangleSolid 5 20
drawSegment 3 = translate     0   (-15) $ rectangleSolid 20 5
drawSegment 4 = translate (-7.5) (-7.5) $ rectangleSolid 5 20
drawSegment 5 = translate (-7.5)   7.5  $ rectangleSolid 5 20
drawSegment 6 =                           rectangleSolid 20 5
drawSegment _ = error "Invalid segment"


drawDigit :: Int -> Picture
drawDigit n = color white $ pictures $ map drawSegment (segments n)
    where segments 0 = [0, 1, 2, 3, 4, 5]
          segments 1 = [1, 2]
          segments 2 = [0, 1, 3, 4, 6]
          segments 3 = [0, 1, 2, 3, 6]
          segments 4 = [1, 2, 5, 6]
          segments 5 = [0, 2, 3, 5, 6]
          segments 6 = [0, 2, 3, 4, 5, 6]
          segments 7 = [0, 1, 2]
          segments 8 = [0, 1, 2, 3, 4, 5, 6]
          segments 9 = [0, 1, 2, 3, 5, 6]
          segments _ = []


drawNumber :: Int -> Picture
drawNumber n = foldl (\x y -> pictures [translate (-25) 0 x, drawDigit y]) Blank (digits n)


-- Given an int representing a bitmap, return a 4x6 picture with the bits representing the white pixels
-- in development...
drawFromBits :: Int -> Picture
drawFromBits n = Blank
