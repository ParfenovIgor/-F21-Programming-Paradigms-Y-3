import Text.Read
import Data.Char (toUpper)

-- 1

-- guess :: (t -> Bool) -> (String -> IO t) -> IO t

-- 2

stringToUpper :: String -> String
stringToUpper x =
    charsToUpper x
    where
        charsToUpper :: [Char] -> [Char]
        charsToUpper [] = []
        charsToUpper (x:xs) = toUpper x : charsToUpper xs

echo :: IO String
echo = do
    line <- getLine
    putStrLn (stringToUpper line)
    echo

main :: IO String
main = echo

-- 3

-- forIO_ :: [IO ()] -> IO ()
-- forIO_ [] = []
-- forIO_ (x : xs) = return x <> forIO_ xs 
