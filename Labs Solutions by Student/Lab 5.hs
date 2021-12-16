import CodeWorld

-- 1

type Mass = Double
type Position = (Double, Double)
type Velocity = (Double, Double)

data Body = MkBody Mass Position Velocity

earth :: Body
earth = (MkBody 0.1 (3, 0) (0, 1))

moon :: Body
moon = (MkBody 0.01 (2, 1) (1, 0))

sun :: Body
sun = (MkBody 1 (0, 0) (0, 0))

data System = MkSystem [Body]

renderBody :: Body -> Picture
renderBody (MkBody m (x, y) (vx, vy)) = translated x y (solidCircle (m ** (1 / 3)))

-- 2

renderBodies :: [Body] -> Picture
renderBodies [] = blank
renderBodies (x:xs) = leftShape <> rightShape
  where
    leftShape = renderBody x
    rightShape = renderBodies xs

renderSystem :: System -> Picture
renderSystem (MkSystem l) = renderBodies l

-- 3

moveBody :: Double -> Body -> Body
moveBody t (MkBody m (x, y) (vx, vy)) = (MkBody m (x + vx * t, y + vy * t) (vx, vy))

-- 4

-- updateBodies :: Double -> [Body] -> [Body]
-- updateBodies t [] = []
-- updateBodies t (x:xs) = (moveBody t x) : (updateBodies t xs)

-- updateSystem :: Double -> System -> System
-- updateSystem t (MkSystem l) = (MkSystem (updateBodies t l))

-- 5

g_c :: Double
g_c = 10

gravityAcc :: Body -> Body -> Vector
gravityAcc (MkBody m1 (x1, y1) (vx1, vy1)) (MkBody m2 (x2, y2) (vx2, vy2)) = 
  if ((x1 - x2) ** 2 + (y1 - y2) ** 2) > 0.01
    then ((x1 - y2) * g_c * m1 / (((x1 - x2) ** 2 + (y1 - y2) ** 2) ** (3/2)), (y1 - y2) * g_c * m1 / (((x1 - x2) ** 2 + (y1 - y2) ** 2) ** (3/2)))
  else
    (0, 0)

-- 6

vectorProduct :: Vector -> Double -> Vector
vectorProduct (a, b) c = (a * c, b * c)

applyGravity :: Double -> [Body] -> Body -> Body
applyGravity t [] body = body
applyGravity t (i:ir) (MkBody m (x, y) (vx, vy)) = (applyGravity t ir (MkBody m (x, y) (vectorSum (vx, vy) (vectorProduct (gravityAcc i (MkBody m (x, y) (vx, vy))) t))))

-- 7

updateBody :: Double -> [Body] -> Body -> Body
updateBody t lst body = (moveBody t (applyGravity t lst body))

updateBodies :: Double -> [Body] -> [Body] -> [Body]
updateBodies t [] bodies = []
updateBodies t (x:xs) bodies = (updateBody t bodies x) : (updateBodies t xs bodies)

updateSystem :: Double -> System -> System
updateSystem t (MkSystem l) = (MkSystem (updateBodies t l l))

main :: IO ()
main = drawingOf (renderSystem (updateSystem 1 (MkSystem [earth, moon, sun])))

