import CodeWorld

import Text.Read

-- Exercise_6_1

exercise_6_1_a :: Integer
exercise_6_1_a = 2 + 3

exercise_6_1_b :: Floating t => (t, t) -> t
exercise_6_1_b (x, y) = sqrt (x^2 + y^2)

exercise_6_1_c :: t -> t -> [t]
exercise_6_1_c x y = [x, y]

exercise_6_1_d :: [Char] -> [Char]
exercise_6_1_d [] = ""
exercise_6_1_d (x:xs) = exercise_6_1_d xs ++ [x]

exercise_6_1_e :: t -> (t, t)
exercise_6_1_e x = (x, x)

-- Exercise_6_2

data Cartesian = Cartesian Double Double
-- data Radians = Radians Double
data Polar = Polar Double Radians

-- toPolar :: Cartesian -> Polar
-- toPolar (Cartesian x y) = Polar (sqrt (x^2 + y^2)) (Radians (atan2 y x))

-- fromPolar :: Polar -> Cartesian
-- fromPolar (Polar r (Radians a)) = Cartesian (r * cos a) (r * sin a)

-- Exercise_6_3

concentric :: Picture -> Int -> Picture
concentric pic 0 = blank
concentric pic n =
  if n `mod` 2 == 0
  then concentric pic (n - 1) <> (colored black (scaled (fromIntegral(n) / 10) (fromIntegral(n) / 10) pic))
  else concentric pic (n - 1) <> (colored red (scaled (fromIntegral(n) / 10) (fromIntegral(n) / 10) pic))

-- Exercise_6_4

type Radians = Double
data Command
  = Forward Double
  | Rotate Radians
  | TeleportTo (Double, Double)

runCommands :: [Command] -> Picture
runCommands lst = helper lst (0, 0) 0
  where
    helper :: [Command] -> (Double, Double) -> Radians -> Picture
    helper [] _ _ = blank
    helper ((Forward len):xs) (x, y) a = (polygon [(x, y), (nx, ny)]) <> helper xs (nx, ny) a
      where
        nx = x + len * cos a
        ny = y + len * sin a
    helper ((Rotate da):xs) pos a = helper xs pos (a + da)
    helper ((TeleportTo (nx, ny)):xs) (x, y) a = helper xs (nx, ny) a
    
-- main = drawingOf (runCommands [Rotate (2*pi/3), Forward 2, Forward (-4), TeleportTo (0, 0), Rotate (2*pi/3), Forward 2])

-- Exercise_6_6(a)

renderCartesianGrid :: (Double, Double) -> (Double, Double) -> Picture
renderCartesianGrid (lx, rx) (ly, ry) = 
  drawVertical (fromIntegral(ceiling(lx)), fromIntegral(floor(rx))) (ly, ry) <>
  drawHorizontal (lx, rx) (fromIntegral(ceiling(ly)), fromIntegral(floor(ry)))
  where
    drawVertical :: (Int, Int) -> (Double, Double) -> Picture
    drawVertical (lx, rx) (ly, ry) =
      if lx <= rx
      then (polygon [(fromIntegral(lx), ly), (fromIntegral(lx), ry)]) <> drawVertical (lx + 1, rx) (ly, ry)
      else blank
      
    drawHorizontal :: (Double, Double) -> (Int, Int) -> Picture
    drawHorizontal (lx, rx) (ly, ry) =
      if ly <= ry
      then (polygon [(lx, fromIntegral(ly)), (rx, fromIntegral(ly))]) <> drawHorizontal (lx, rx) (ly + 1, ry)
      else blank

-- Exercise_6_6(b)

renderPolarGrid :: Double -> Int -> Picture
renderPolarGrid r nsec =
  drawCircles (fromIntegral(floor(r))) <>
  drawLines (nsec * 4) (pi / (fromIntegral(nsec) * 2)) 0 r
  where
    drawCircles :: Int -> Picture
    drawCircles 0 = blank
    drawCircles n = (circle (fromIntegral(n))) <> (drawCircles (n - 1))
    
    drawLines :: Int -> Double -> Double -> Double -> Picture
    drawLines 0 _ _ _ = blank
    drawLines n da a r = (polygon [(0, 0), (r * cos a, r * sin a)]) <> drawLines (n - 1) da (a + da) r

main :: IO ()
main = drawingOf (renderPolarGrid 5.5 5)

-- main :: IO ()
-- main = drawingOf (renderCartesianGrid (-5.5, 5.5) (-5.5, 5.5))
