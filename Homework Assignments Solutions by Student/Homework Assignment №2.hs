{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.Maybe
import CodeWorld

-- i.parfenov@innopolis.university

data Line a = Line [a] a [a]
  deriving (Show)

integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- | Exercise 1.1

cutLine :: Int -> Line a -> Line a
cutLine d (Line l c r) = Line (cut d l) c (cut d r)
  where
    cut :: Int -> [a] -> [a]
    cut 0 _ = []
    cut d (x:xs) = x:cut (d-1) xs

-- main = print (cutLine 3 integers)

-- | Exercise 1.2

genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f c g = Line (gen f (f c)) c (gen g (g c))
  where
    gen :: (a -> Maybe a) -> Maybe a -> [a]
    gen _ Nothing = []
    gen f (Just c) = c:gen f (f c)

add :: Int -> Maybe Int
add a =
  if a >= 5
    then Nothing
  else
    Just (a + 1)

-- main = print (genLine add 0 add)

-- | Exercise 1.3

mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line l c r) = Line (map f l) (f c) (map f r)

-- main = print (cutLine 5 (mapLine (^2) integers))

-- | Exercise 1.4

zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line la ca ra) (Line lb cb rb) = Line (zip la lb) (ca, cb) (zip ra rb)

-- main = print (cutLine 5 (zipLines integers integers))

zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line la ca ra) (Line lb cb rb) = Line (zipWith f la lb) (f ca cb) (zipWith f ra rb)

-- main = print (cutLine 5 (zipLinesWith (*) integers integers))

-- | Exercise 1.5

data Cell = Alive | Dead
  deriving (Show, Eq)

rule30 :: Line Cell -> Cell
rule30 (Line (Dead:l) Dead (Dead:r))    = Dead
rule30 (Line []       Dead (Dead:r))    = Dead
rule30 (Line (Dead:l) Dead [])          = Dead
rule30 (Line []       Dead [])          = Dead

rule30 (Line (Dead:l) Dead (Alive:r))   = Alive
rule30 (Line []       Dead (Alive:r))   = Alive

rule30 (Line (Dead:l) Alive (Dead:r))   = Alive
rule30 (Line []       Alive (Dead:r))   = Alive
rule30 (Line (Dead:l) Alive [])         = Alive
rule30 (Line []       Alive [])         = Alive

rule30 (Line (Dead:l) Alive (Alive:r))  = Alive
rule30 (Line [] Alive (Alive:r))        = Alive

rule30 (Line (Alive:l) Dead (Dead:r))   = Alive
rule30 (Line (Alive:l) Dead [])         = Alive

rule30 (Line (Alive:l) Dead (Alive:r))  = Dead

rule30 (Line (Alive:l) Alive (Dead:r))  = Dead

rule30 (Line (Alive:l) Alive (Alive:r)) = Dead

rule30 _ = Dead

-- main = print (rule30 (Line [Dead] Alive [Dead]))
-- main = print (rule30 (Line [Alive] Alive [Dead]))

-- | Exercise 1.6

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] c r) = Nothing
shiftLeft (Line (l1:l2) c r) = Just (Line l2 l1 (c:r))

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line l c []) = Nothing
shiftRight (Line l c (r1:r2)) = Just (Line (c:l) r1 r2)

{-
main = print (cutLine 5 (expand (shiftLeft integers)))
  where
    expand :: Maybe (Line a) -> Line a
    expand (Just line) = line
-}

{-
main = print (cutLine 5 (expand (shiftRight integers)))
  where
    expand :: Maybe (Line a) -> Line a
    expand (Just line) = line
-}

-- | Exercise 1.7

lineShifts :: Line a -> Line (Line a)
lineShifts line = Line (moveLeft (shiftLeft line)) line (moveRight (shiftRight line))
  where
    moveLeft :: Maybe (Line a) -> [Line a]
    moveLeft Nothing = []
    moveLeft (Just line) = line:moveLeft (shiftLeft line)

    moveRight :: Maybe (Line a) -> [Line a]
    moveRight Nothing = []
    moveRight (Just line) = line:moveRight (shiftRight line)

-- main = print (cutLine 5 (lineShifts (cutLine 5 integers)))

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- main = print (applyRule30 (Line [Dead, Dead] Alive [Dead, Dead]))

-- | Exercise 1.8

renderLine :: Line Picture -> Picture
renderLine (Line l c r) = render l (-1) (-1) <> c <> render r 1 1
  where
    render :: [Picture] -> Double -> Double -> Picture
    render [] _ _ = blank
    render (p:ps) x dx = translated x 0 p <> render ps (x + dx) dx
    
-- main = drawingOf (renderLine (Line [rectangle 1 1, rectangle 1 1] (solidRectangle 1 1) [rectangle 1 1, rectangle 1 1]))

lineCellToPicture :: Line Cell -> Line Picture
lineCellToPicture (Line l c r) = Line (listCellToPictures l) (cellToPicture c) (listCellToPictures r)
  where
    cellToPicture :: Cell -> Picture
    cellToPicture Alive = solidRectangle 1 1
    cellToPicture Dead = rectangle 1 1
  
    listCellToPictures :: [Cell] -> [Picture]
    listCellToPictures [] = []
    listCellToPictures (p:ps) = cellToPicture p : listCellToPictures ps

renderRule30 :: Int -> Line Cell -> Picture
renderRule30 n line = 
  if n == 0
  then renderLine (lineCellToPicture line)
  else translated 0 (fromIntegral n) (renderLine (lineCellToPicture line)) <> renderRule30 (n - 1) (applyRule30 line)

cleanTape :: Int -> Line Cell
cleanTape 0 = (Line [] Alive [])
cleanTape n = append (cleanTape (n-1))
  where
    append :: Line Cell -> Line Cell
    append (Line l c r) = (Line (Dead:l) c (Dead:r))

-- main = drawingOf (renderRule30 10 (cleanTape 12))

-- | Exercise 1.9

data Space a = Space (Line (Line a))
  deriving (Show)

productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line l c r) line2 = Space (Line (applyLineToList l line2) (applyElementToLine c line2) (applyLineToList r line2))
  where
    applyElementToList :: a -> [b] -> [(a, b)]
    applyElementToList _ [] = []
    applyElementToList x (l:ls) = (x, l) : applyElementToList x ls

    applyElementToLine :: a -> Line b -> Line (a, b)
    applyElementToLine x (Line l c r) = Line (applyElementToList x l) (x, c) (applyElementToList x r)

    applyLineToList :: [a] -> Line b -> [Line (a, b)]
    applyLineToList [] _ = []
    applyLineToList (x:xs) line = applyElementToLine x line : applyLineToList xs line

exampleLine :: Line Int
exampleLine = Line [-1, -2, -3] 0 [1, 2, 3]

-- main = print (productOfLines exampleLine exampleLine)

-- | Exercise 1.10

mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line l c r)) = Space (Line (map (mapLine f) l) (mapLine f c) (map (mapLine f) r))

{-
main = print (mapSpace sumOfPair (productOfLines exampleLine exampleLine))
  where
    sumOfPair :: (Int, Int) -> Int
    sumOfPair (a, b) = a + b
-}

zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line l1 c1 r1)) (Space (Line l2 c2 r2)) = Space (Line (zipWith zipLines l1 l2) (zipLines c1 c2) (zipWith zipLines r1 r2))

exampleSpace :: Space Int
exampleSpace = Space (Line
                        [
                           Line [0, -1]    1  [2, 3]
                        ]
                          (Line [-1, -2]   0  [1, 2])
                        [
                           Line [-2, -3] (-1) [0, 1]
                        ]
                     )

-- main = print (zipSpaces exampleSpace exampleSpace)

zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f (Space (Line l1 c1 r1)) (Space (Line l2 c2 r2)) = Space (Line (zipWith (zipLinesWith f) l1 l2) (zipLinesWith f c1 c2) (zipWith (zipLinesWith f) r1 r2))

{-
main = print (zipSpacesWith mult exampleSpace exampleSpace)
  where
    mult :: Int -> Int -> Int
    mult a b = a * b
-}

-- | Exercise 1.12

conwayRule :: Space Cell -> Cell
conwayRule (Space (Line l (Line lc cc rc) r)) = do
  let total = countUp space
            + countDown space
            + countLeft space
            + countRight space
            + countLeftUp space
            + countLeftDown space
            + countRightUp space
            + countRightDown space
  if cc == Alive
    then
    if total == 2 || total == 3
    then Alive
    else Dead
  else
    if total == 3
    then Alive
    else Dead

  where
    space :: Space Cell
    space = Space (Line l (Line lc cc rc) r)

    countUp :: Space Cell -> Int
    countUp (Space (Line _ (Line (Alive:_) _ _) _)) = 1
    countUp _ = 0

    countDown :: Space Cell -> Int
    countDown (Space (Line _ (Line _ _ (Alive:_)) _)) = 1
    countDown _ = 0

    countLeft :: Space Cell -> Int
    countLeft (Space (Line ((Line _ Alive _):_) _ _)) = 1
    countLeft _ = 0

    countRight :: Space Cell -> Int
    countRight (Space (Line _ _ ((Line _ Alive _):_))) = 1
    countRight _ = 0

    countLeftUp :: Space Cell -> Int
    countLeftUp (Space (Line ((Line (Alive:_) _ _):_) _ _)) = 1
    countLeftUp _ = 0

    countLeftDown :: Space Cell -> Int
    countLeftDown (Space (Line ((Line _ _ (Alive:_)):_) _ _)) = 1
    countLeftDown _ = 0

    countRightUp :: Space Cell -> Int
    countRightUp (Space (Line _ _ ((Line (Alive:_) _ _):_))) = 1
    countRightUp _ = 0

    countRightDown :: Space Cell -> Int
    countRightDown (Space (Line _ _ ((Line _ _ (Alive:_)):_))) = 1
    countRightDown _ = 0

exampleCellSpace :: Space Cell
exampleCellSpace = Space (Line
                        [
                          Line [Dead, Alive] Dead [Alive, Alive]
                        ]
                          (Line [Alive, Alive] Alive [])
                        [
                        ]
                     )

-- main = print (conwayRule exampleCellSpace)

-- | Exercise 1.13

shiftLeftSpace :: Space a -> Maybe (Space a)
shiftLeftSpace (Space (Line [] c r)) = Nothing
shiftLeftSpace (Space (Line (l1:l2) c r)) = Just (Space (Line l2 l1 (c:r)))

shiftRightSpace :: Space a -> Maybe (Space a)
shiftRightSpace (Space (Line l c [])) = Nothing
shiftRightSpace (Space (Line l c (r1:r2))) = Just (Space (Line (c:l) r1 r2))

shiftUpSpace :: Space a -> Maybe (Space a)
shiftUpSpace (Space (Line l c r)) =
  if isNothing (shiftUpList l) || isNothing (shiftLeft c) || isNothing (shiftUpList r)
  then Nothing
  else Just (Space (Line 
    (applyJust (shiftUpList l))
    (applyJust (shiftLeft c))
    (applyJust (shiftUpList r))
  ))
  where
    shiftUpList :: [Line a] -> Maybe [Line a]
    shiftUpList [] = Just []
    shiftUpList (x:xs) =
      if isNothing (shiftLeft x) || isNothing (shiftUpList xs)
      then Nothing
      else Just (applyJust (shiftLeft x) : applyJust (shiftUpList xs))

    applyJust :: Maybe a -> a
    applyJust (Just x) = x

shiftDownSpace :: Space a -> Maybe (Space a)
shiftDownSpace (Space (Line l c r)) =
  if isNothing (shiftDownList l) || isNothing (shiftRight c) || isNothing (shiftDownList r)
  then Nothing
  else Just (Space (Line 
    (applyJust (shiftDownList l))
    (applyJust (shiftRight c))
    (applyJust (shiftDownList r))
  ))
  where
    shiftDownList :: [Line a] -> Maybe [Line a]
    shiftDownList [] = Just []
    shiftDownList (x:xs) =
      if isNothing (shiftRight x) || isNothing (shiftDownList xs)
      then Nothing
      else Just (applyJust (shiftRight x) : applyJust (shiftDownList xs))

    applyJust :: Maybe a -> a
    applyJust (Just x) = x

{-
main = print (expand (shiftLeftSpace exampleSpace))
  where
    expand :: Maybe (Space a) -> Space a
    expand (Just space) = space
-}

{-
main = print (expand (shiftUpSpace exampleSpace))
  where
    expand :: Maybe (Space Int) -> Space Int
    expand (Just space) = space
    expand Nothing = Space (Line [] (Line [] 42 []) [])
-}

spaceShifts :: Space a -> Space (Space a)
spaceShifts space = Space (Line (moveLeft (shiftLeftSpace space)) (moveVert space) (moveRight (shiftRightSpace space)))
  where
    moveLeft :: Maybe (Space a) -> [Line (Space a)]
    moveLeft Nothing = []
    moveLeft (Just (Space line)) = moveVert (Space line) : moveLeft (shiftLeftSpace (Space line))

    moveRight :: Maybe (Space a) -> [Line (Space a)]
    moveRight Nothing = []
    moveRight (Just (Space line)) = moveVert (Space line) : moveRight (shiftRightSpace (Space line))

    moveVert :: Space a -> Line (Space a)
    moveVert space = Line (moveUp (shiftUpSpace space)) space (moveDown (shiftDownSpace space))

    moveUp :: Maybe (Space a) -> [Space a]
    moveUp Nothing = []
    moveUp (Just (Space line)) = Space line : moveUp (shiftUpSpace (Space line))

    moveDown :: Maybe (Space a) -> [Space a]
    moveDown Nothing = []
    moveDown (Just (Space line)) = Space line : moveDown (shiftDownSpace (Space line))

-- main = print (spaceShifts exampleSpace)

applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

startCellSpace :: Int -> Space Cell
startCellSpace n = Space (Line (buildCellListOfLines n n 1) (buildCellLine n 0) (buildCellListOfLines n n 1))
  where
    buildCell :: Int -> Cell
    buildCell 0 = Dead
    buildCell 1 = Alive
    
    buildCellList :: Int -> Int -> [Cell]
    buildCellList 0 _ = []
    buildCellList n m = buildCell m : buildCellList (n - 1) (1 - m)
  
    buildCellLine :: Int -> Int -> Line Cell
    buildCellLine n m = Line (buildCellList n (1 - m)) (buildCell m) (buildCellList n (1 - m))
    
    buildCellListOfLines :: Int -> Int -> Int -> [Line Cell]
    buildCellListOfLines _ 0 _ = []
    buildCellListOfLines n s m = buildCellLine n m : buildCellListOfLines n (s - 1) (1 - m)

-- main = print (applyConwayRule (startCellSpace 3))

-- | Exercise 1.14

renderSpace :: Space Picture -> Picture
renderSpace (Space (Line l c r)) = render l (-1) (-1) <> (renderLine c) <> render r 1 1
  where
    render :: [Line Picture] -> Double -> Double -> Picture
    render [] _ _ = blank
    render (p:ps) x dx = translated 0 x (renderLine p) <> render ps (x + dx) dx

spaceCellToPicture :: Space Cell -> Space Picture
spaceCellToPicture (Space (Line l c r)) = Space (Line (map lineCellToPicture l) (lineCellToPicture c) (map lineCellToPicture r))
  
-- main = drawingOf (renderSpace (spaceCellToPicture (startCellSpace 5)))

-- main = drawingOf (renderSpace (spaceCellToPicture (applyConwayRule (startCellSpace 5))))

animateConway :: Space Cell -> IO ()
animateConway space = animationOf animate
  where
    animate :: Double -> Picture
    animate seconds = renderSpace (spaceCellToPicture (applyConwayRuleN (startCellSpace 5) (fromIntegral (floor seconds))))
    
    applyConwayRuleN :: Space Cell -> Int -> Space Cell
    applyConwayRuleN space 0 = space
    applyConwayRuleN space n = applyConwayRuleN (applyConwayRule space) (n - 1)

main = animateConway (startCellSpace 5)
