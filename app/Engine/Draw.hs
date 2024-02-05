module Draw where


import Data.Char
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
drawSegment 7 = polygon [(-5, -2.5), (0, -2.5), (10, -12.5), (10, -17.5), (5, -17.5), (-5, -7.5)]
drawSegment 8 = polygon [(0, -7.5), (5, -12.5), (5, -17.5), (0, -12.5), (-5, -17.5), (-5, -12.5)]
drawSegment 9 =                           rectangleSolid 5 35
drawSegment 10 = polygon [(0, -2.5), (10, -12.5), (5, -12.5), (0, -7.5)]
drawSegment _ = Blank


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


drawLetter :: Char -> Picture
drawLetter c = color white $ pictures $ map drawSegment (segments c)
    where segments 'p' = [0, 1, 4, 5, 6]
          segments 'l' = [3, 4, 5]
          segments 'a' = [0, 1, 2, 4, 5, 6]
          segments 'y' = [1, 2, 3, 5, 6]
          segments 'e' = [0, 3, 4, 5, 6]
          segments 'r' = [0, 1, 4, 5, 6, 7]
          segments 'w' = [1, 2, 4, 5, 8]
          segments 'i' = [1, 2]
          segments 'n' = [0, 1, 2, 4, 5]
          segments 's' = [0, 2, 3, 5, 6]
          segments 'c' = [0, 3, 4, 5]
          segments 't' = [0, 9]
          segments 'o' = [0, 1, 2, 3, 4, 5]
          segments 'q' = [0, 1, 2, 3, 4, 5, 10]
          segments 'u' = [1, 2, 3, 4, 5]
          segments 'g' = [0, 2, 3, 4, 5]
          segments  _  = []


drawChar :: Char -> Picture
drawChar c | isDigit c = drawDigit (digitToInt c)
           | otherwise = drawLetter c


-- drawNumber and drawString are centered on the last character's center
drawNumber :: Int -> Picture
drawNumber n = foldl (\x y -> pictures [translate (-25) 0 x, drawDigit y]) Blank (digits n)


drawString :: String -> Picture
drawString = foldl (\x y -> pictures [translate (-25) 0 x, drawChar y]) Blank


-- Given an int representing a bitmap, return a 4x6 picture with the bits representing the white pixels
-- in development...
drawFromBits :: Int -> Picture
drawFromBits n = Blank
