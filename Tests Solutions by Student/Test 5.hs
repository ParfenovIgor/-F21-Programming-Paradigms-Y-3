propagate :: (Bool,[Int]) -> [(Bool,Int)]
propagate (t, []) = []
propagate (t,(x:xs)) = (t, x) : propagate (t,xs)

data Radians = Radians Double
data Degrees = Degrees Double

-- pi :: Double
-- pi = 3.14159

toDegrees :: Radians -> Degrees
toDegrees (Radians x) = (Degrees (x * 180 / pi))

toRadians :: Degrees -> Radians
toRadians (Degrees x) = (Radians (x / 180 * pi))

main = print (propagate (True, [1, 2, 3]))
