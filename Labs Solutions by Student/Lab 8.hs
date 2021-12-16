{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random

-- | Distance in units.
type Distance = Double

type Stars = [(Double,Double)]

-- | Game state only contains:
-- * distance travelled
-- * ???
-- | Exercise 6.4.
-- data Game = Game Distance [Stars]
-- | Exercise 6.5.
data Game = Game Distance Int

-- | Speed of player moving through the universe.
gameSpeed :: Double
gameSpeed = 10

-- | Universe is split into blocks.
-- This is the size of each block.
blockWidth :: Double
blockWidth = 20

-- | Initial state of the game.
initGame :: Game
-- initGame = Game 0 [starsPositions]
initGame = Game 0 1

-- | Event dispatcher.
handleGame :: Event -> Game -> Game
handleGame (TimePassing dt) = updateGame dt
handleGame _event = id

-- | With time player moves through the universe.
updateGame :: Double -> Game -> Game
updateGame dt (Game distance back)
  = Game (distance + dt * gameSpeed) back
  
-- | Exercise 6.2.

starsPositions :: Stars
starsPositions = [(-10, 4),(-8, -6),(-6, 2),(-4, -10),(-2, 8),(0, -4),(2, 10),(4, 0),(6, -2),(8, 6),(10, -8)]

starsPositions2 :: Stars
starsPositions2 = [(-10, 6),(-8, -2),(-6, 4),(-4, -10),(-2, 2),(0, -6),(2, 6),(4, -10),(6, 0),(8, 10),(10, -4)]

starsPositions3 :: Stars
starsPositions3 = [(-10, 10),(-8, 8),(-6, 6),(-4, 4),(-2, 2),(0, 0),(2, -2),(4, -4),(6, -6),(8, -8),(10, -10)]

randomStarsArrangement :: Stars
randomStarsArrangement = starsPositions

infiniteStarsArrangements :: Int -> [Stars]
infiniteStarsArrangements n = starsPositions3 : infiniteStarsArrangements (n-1)

-- | Exercise 6.6.
chunksOf :: Int -> [a] -> [[a]]
chunksOf sz lst = helper sz 0 lst []
  where
    helper :: Int -> Int -> [a] -> [a] -> [[a]]
    helper sz _ [] [] = []
    helper sz _ [] lstCur = [lstCur]
    helper sz curSz (x:xs) lstCur =
      if curSz == sz-1
      then (lstCur ++ [x]) : helper sz 0 xs []
      else helper sz (curSz+1) xs (lstCur ++ [x])

drawStars :: Stars -> Picture
drawStars [] = blank
drawStars ((x,y):xs) = (translated x y (colored white (solidCircle 0.1))) <> drawStars xs

-- | Background picture with some mountains.
background :: Picture
background = stars <> solidRectangle (blockWidth+1) 30
  -- | Exercise 6.1.
  where
    stars = drawStars starsPositions
    -- stars = (translated 3 3 (colored white (solidCircle 0.3)))

-- | Exercise 6.3.
background2 :: Picture
background2 = stars <> solidRectangle (blockWidth+1) 30
  where
    stars = drawStars starsPositions2

-- | Exercise 6.4.
backgroundStars :: Stars -> Picture
backgroundStars stars = drawStars stars <> solidRectangle (blockWidth+1) 30

-- | Rendering a picture of the player.
renderPlayer :: Picture
renderPlayer = rotated (-0.7) (scaled 3 3 (lettering "\x1F680"))

-- | We are rendering only four blocks of the universe.
-- Everything else is either too far in the past
-- or too far in the future,
-- and cannot be visible anyway.
-- | Exercise 6.3.
-- renderGame :: Game -> Picture
-- renderGame (Game distance) = translated (-5) 0 $ pictures
  -- [ renderPlayer
  -- , translated ( -blockWidth + dx) 0 background
  -- , translated                 dx  0 background2
  -- , translated (  blockWidth + dx) 0 background
  -- , translated (2*blockWidth + dx) 0 background2
  -- , translated (3*blockWidth + dx) 0 background
  -- , translated (4*blockWidth + dx) 0 background2
  -- ]
  -- where
    -- dx = - (distance - fromIntegral (floor (distance / (2 * blockWidth))) * (2 * blockWidth))

-- | Exercise 6.4.
renderGame :: Game -> Picture
-- renderGame (Game distance back) = translated (-5) 0 (renderPlayer <> (helper ((length back) * 2) (back ++ back ++ back ++ back)))
-- | Exercise 6.5.
renderGame (Game distance len) = translated (-5) 0 (renderPlayer <> (helper (len * 2 + 1)))
   where
    helper :: Int -> Picture
    helper (-1) = blank
    helper block = (translated (fromIntegral(block)*blockWidth + dx (fromIntegral(len))) 0 (backgroundStars ((infiniteStarsArrangements 1)!!block))) <> helper (block - 1)
   
    dx :: Double -> Double
    dx len = - (distance - fromIntegral (floor (distance / (len * blockWidth))) * (len * blockWidth))

-- | Program entry point.
main :: IO ()
main = activityOf initGame handleGame renderGame
