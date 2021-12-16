import System.Posix.Internals (lstat)
import Distribution.Fields.LexerMonad (LexState(curCode))
-- 1
k :: (t -> String) -> [t] -> [IO ()]
k g = map (\x -> putStrLn (g x))

-- 2

splitResults :: [Result a] -> ([String], [a])
splitResults lst = (getFailures (reverse lst) [], getSuccesses (reverse lst) [])
  where
    getFailures :: [Result a] -> [String] -> [String]
    getFailures [] cur = cur
    getFailures ((Failure x):xs) cur = getFailures xs (x:cur)
    getFailures (x:xs) cur = getFailures xs cur

    getSuccesses :: [Result a] -> [a] -> [a]
    getSuccesses [] cur = cur
    getSuccesses ((Success x):xs) cur = getSuccesses xs (x:cur)
    getSuccesses (x:xs) cur = getSuccesses xs cur

data Result a = Success a | Failure String deriving Show

divide :: Int -> Int -> Result Int
divide _ 0 = Failure "division by zero"
divide n m = Success (n `div` m)

example2 = splitResults (zipWith divide [6, 7, 8] [3, 0, 2])

-- 3

data Grid a = Grid [[a]] deriving Show

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f (Grid a) = Grid (calc1 f a)
  where
    calc1 :: (a -> b) -> [[a]] -> [[b]]
    calc1 f [] = []
    calc1 f (x:xs) = (calc2 f x):(calc1 f xs)

    calc2 :: (a -> b) -> [a] -> [b]
    calc2 f [] = []
    calc2 f (x:xs) = (f x):(calc2 f xs)

double :: Int -> Int 
double x = x * 2

example3 = mapGrid double (Grid [[1, 2], [3, 4]])

-- 4

enumerateGrid :: Grid a -> Grid (Int, a)
enumerateGrid (Grid a) = Grid (calc1 1 a)
  where
    calc1 :: Int -> [[a]] -> [[(Int, a)]]
    calc1 num [] = []
    calc1 num (x:xs) = (calc2 num x):(calc1 (num + (length x)) xs)

    calc2 :: Int -> [a] -> [(Int, a)]
    calc2 num [] = []
    calc2 num (x:xs) = (num, x):(calc2 (num + 1) xs)

example4 = enumerateGrid (Grid [['a', 'b'], ['c', 'd']])
main = print example4