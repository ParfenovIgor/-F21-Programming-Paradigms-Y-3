import Text.Read

-- | Exercise 6.1.

getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    case readMaybe line of
        Nothing -> do
            return Nothing
        n -> return n

sumTwoNumber :: IO ()
sumTwoNumber = do
    x <- getInt
    y <- getInt
    printer x y
    where
        printer :: Maybe Int -> Maybe Int -> IO ()
        printer (Just x) (Just y) = print (x + y)

-- main :: IO ()
-- main = sumTwoNumber

-- | Exercise 6.2.

replicate':: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

replicateIO :: Maybe Int -> IO a -> IO [a]
replicateIO (Just 0) _ = return []
replicateIO (Just n) program = do
    x <- program
    xs <- replicateIO (Just (n-1)) program
    return (x : xs)

sumManyNumbers :: IO()
sumManyNumbers = do
    n <- getInt
    ns <- replicateIO n getInt
    print (sum ns)
    where
        sum :: [Maybe Int] -> Int
        sum [] = 0
        sum ((Just x):xs) = x + sum xs

-- main :: IO ()
-- main = sumManyNumbers

-- | Exercise 6.3.

type Answer = Int
data Question = Question String Answer

runQuestion :: Question -> IO (Maybe Answer)
runQuestion (Question text _) = do
    putStrLn text
    res <- getInt
    helper res
    where
        helper :: Maybe Int -> IO (Maybe Answer)
        helper Nothing = return Nothing
        helper a = return a

-- main :: IO (Maybe Answer)
-- main = runQuestion (Question "1 + 2 = " 3)

-- | Exercise 6.4.

applyToArray :: (a -> IO b) -> [a] -> IO [b]
applyToArray f [] =
    return []
applyToArray f (x:xs) = do
    _x <- f x
    _xs <- applyToArray f xs
    return (_x:_xs)

runQuestions :: [Question] -> IO [Maybe Answer]
runQuestions = applyToArray runQuestion

-- main :: IO [Maybe Answer]
-- main = runQuestions [Question "1 + 2 = " 3, Question "2 * 3 = " 6, Question "4 ^ 2 = " 16]
