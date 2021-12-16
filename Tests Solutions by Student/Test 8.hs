import Text.Read

-- 1

f :: ([t] -> [t]) -> t -> [t] -> [t]
f x y z = x (y : x z)
-- Apply x to the list, () is list constructor
-- Apply x to z, hence z is list with same type t
-- Cons y to begining of the list, hence y is type t
-- f = apply x to list of t, hence it is -> [t]

-- 2

repeatUntil :: (a -> Bool) -> (a -> IO a) -> a -> IO a
repeatUntil p f x =
  if p x
  then return x
  else do 
    x_ <- f x
    repeatUntil p f x_

tryGetInt :: IO (Maybe Int)
tryGetInt = do
  line <- getLine 
  return (readMaybe line)

example1 :: IO (Maybe Int)
example1 = repeatUntil (< Just 10) (\x -> tryGetInt) Nothing

-- 3

type Point = (Double, Double)
data Event = Click | Move Point

toPolyline :: [Event] -> [Point]
toPolyline lst = helper lst 0 0
    where
      helper [] _ _ = []
      helper ((Move (dx, dy)):xs) x y = helper xs (x + dx) (y + dy)
      helper (Click:xs) x y = (x, y) : helper xs x y

example3 :: [Point]
example3 = toPolyline [Move (1, 2), Move (3, 4), Click, Move (5, 6), Click, Click]

-- 5

prefixes :: [a] -> [[a]]
prefixes lst = helper lst []
  where
    helper :: [a] -> [a] -> [[a]]
    helper [] cur = [cur]
    helper (x:xs) cur = cur : helper xs (cur ++ [x])

example5 = prefixes "hello"

