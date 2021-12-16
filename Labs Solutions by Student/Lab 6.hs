{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | A grid represented as a list of rows of "things".
data Grid a = Grid [[a]]

-- * A grid of pictures

-- | Picture of a wall.
wallPicture :: Picture
wallPicture = solidRectangle 0.95 0.95

-- | Picture of a floor.
floorPicture :: Picture
floorPicture = colored (light (light gray)) (solidRectangle 0.95 0.95)

-- | Doors will be differentiated by their index.
type DoorId = Int

-- | Doors and keys are distinguished visually using color.
doorIdColor :: DoorId -> Color
doorIdColor 0 = red
doorIdColor 1 = blue
doorIdColor 2 = green
doorIdColor n = light (doorIdColor (n - 3))

-- | Picture of a door with a given index.
doorPicture :: DoorId -> Picture
doorPicture doorId
  = colored (doorIdColor doorId) (solidCircle 0.3)
 <> wallPicture

-- | Picture of a key for a door with a given index.
keyPicture :: DoorId -> Picture
keyPicture doorId
  = scaled 0.5 0.5 (lettering "🔑")
 <> colored (doorIdColor doorId) (solidCircle 0.42)
 <> floorPicture 

-- | Picture of a coin.
coinPicture :: Picture
coinPicture = scaled 0.7 0.7 (lettering "🍎") <> floorPicture

-- | A sample grid of pictures.
myPictureGrid :: Grid Picture
myPictureGrid = Grid
  [ [ w, w, w, w, w, w, w, w, w ]
  , [ w, c, w, f, f, f, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, f, f, w, w, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, w, w, w, w, d, w, w ]
  , [ w, f, w, c, w, f, f, f, w ]
  , [ w, k, w, f, f, f, w, c, w ]
  , [ w, w, w, w, w, w, w, w, w ]
  ]
  where
    w = wallPicture
    f = floorPicture
    k = keyPicture 1
    d = doorPicture 1
    c = coinPicture
    
-- | Exercise 6.1.
-- Implement this function. Try using higher-order functions.
renderGrid :: Grid Picture -> Picture
renderGrid g = renderAll 0 g
  where
    renderAll :: Double -> Grid Picture -> Picture
    renderAll y (Grid []) = blank
    renderAll y (Grid (row:other)) = (renderRow 0 y row) <> (renderAll (y - 1) (Grid other))

    renderRow :: Double -> Double -> [Picture] -> Picture
    renderRow x y [] = blank
    renderRow x y (pic:other) = (translated x y pic) <> (renderRow (x + 1) y other)

-- * User-defined Tiles

-- | An item that can be placed on a floor tile.
data Item
  = Key DoorId  -- ^ A key for some door.
  | Coin        -- ^ A coin.

-- | A tile.
data Tile
  = Wall                -- ^ A wall tile.
  | Floor (Maybe Item)  -- ^ A floor tile, possibly with some item on it.
  | Door DoorId         -- ^ A door (with its index).

-- | A sample grid of Tiles.
myTileGrid :: Grid Tile
myTileGrid = Grid
  [ [ w, w, w, w, w, w, w, w, w ]
  , [ w, c, w, f, f, f, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, f, f, w, w, w, f, w ]
  , [ w, f, w, f, w, f, f, f, w ]
  , [ w, f, w, w, w, w, d, w, w ]
  , [ w, f, w, c, w, f, f, f, w ]
  , [ w, k, w, f, f, f, w, c, w ]
  , [ w, w, w, w, w, w, w, w, w ]
  ]
  where
    w = Wall
    f = Floor Nothing
    k = Floor (Just (Key 1))
    d = Door 1
    c = Floor (Just Coin)

-- | Exercise 6.2(a).
-- Implement this function.
renderItem :: Maybe Item -> Picture
renderItem Nothing = blank
renderItem (Just (Key i)) = keyPicture i
renderItem (Just Coin) = coinPicture

-- | Exercise 6.2(b).
-- Implement this function.
renderTile :: Tile -> Picture
renderTile (Wall) = wallPicture
renderTile (Floor Nothing) = floorPicture
renderTile (Floor x) = renderItem x
renderTile (Door i) = doorPicture i

-- | Exercise 6.3.
-- Implement this function. Try using higher-order functions.
renderTileGrid :: Grid Tile -> Picture
renderTileGrid g = renderTileAll 0 g
  where
    renderTileAll :: Double -> Grid Tile -> Picture
    renderTileAll y (Grid []) = blank
    renderTileAll y (Grid (row:other)) = (renderTileRow 0 y row) <> (renderTileAll (y - 1) (Grid other))

    renderTileRow :: Double -> Double -> [Tile] -> Picture
    renderTileRow x y [] = blank
    renderTileRow x y (pic:other) = (translated x y (renderTile pic)) <> (renderTileRow (x + 1) y other)

-- | Exercise 6.4.
-- Implement this function. Try using higher-order functions.
removeItems :: Grid Tile -> Grid Tile
removeItems (Grid g) = (Grid (removeItemsAll g))
  where
    removeFromFloor :: Tile -> Tile
    removeFromFloor (Wall) = Wall
    removeFromFloor (Floor Nothing) = (Floor Nothing)
    removeFromFloor (Floor x) = (Floor Nothing)
    removeFromFloor (Door i) = (Door i)

    removeItemsAll :: [[Tile]] -> [[Tile]]
    removeItemsAll [] = []
    removeItemsAll (row:other) = ((removeItemsRow row) : (removeItemsAll other))

    removeItemsRow :: [Tile] -> [Tile]
    removeItemsRow [] = []
    removeItemsRow (it:other) = (removeFromFloor it) : (removeItemsRow other)
  
-- | Exercise 6.5.
-- Implement this function. Try using higher-order functions.
mapGrid :: Grid Tile -> (Tile -> Tile) -> Grid Tile
mapGrid (Grid g) f = (Grid (mapAll g f))
  where
    mapAll :: [[Tile]] -> (Tile -> Tile) -> [[Tile]]
    mapAll [] f = []
    mapAll (row:other) f = ((mapRow row f) : (mapAll other f))

    mapRow :: [Tile] -> (Tile -> Tile) -> [Tile]
    mapRow [] f = []
    mapRow (it:other) f = (f it) : (mapRow other f)

-- | Exercise 6.6.
-- Implement this function. Try using higher-order functions.
openDoors :: Grid Tile -> Grid Tile
openDoors (Grid g) = (Grid (openDoorsAll g))
  where
    openDoor :: Tile -> Tile
    openDoor (Wall) = Wall
    openDoor (Floor Nothing) = (Floor Nothing)
    openDoor (Floor x) = (Floor x)
    openDoor (Door i) = (Floor Nothing)

    openDoorsAll :: [[Tile]] -> [[Tile]]
    openDoorsAll [] = []
    openDoorsAll (row:other) = ((openDoorsRow row) : (openDoorsAll other))

    openDoorsRow :: [Tile] -> [Tile]
    openDoorsRow [] = []
    openDoorsRow (it:other) = (openDoor it) : (openDoorsRow other)

-- | Exercise 6.7.
-- Implement this function. Try using higher-order functions.
myCharGrid :: Grid Char
myCharGrid = Grid
  [ [ 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w' ]
  , [ 'w', 'c', 'w', 'f', 'f', 'f', 'w', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'f', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'f', 'f', 'f', 'w', 'w', 'w', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'f', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'f', 'w', 'w', 'w', 'w', 'd', 'w', 'w' ]
  , [ 'w', 'f', 'w', 'c', 'w', 'f', 'f', 'f', 'w' ]
  , [ 'w', 'k', 'w', 'f', 'f', 'f', 'w', 'c', 'w' ]
  , [ 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w' ]
  ]

myTileGrid2 :: Grid Char -> Grid Tile
myTileGrid2 (Grid g) = (Grid (getAll g))
  where
    getTile :: Char -> Tile
    getTile 'w' = Wall
    getTile 'f' = Floor Nothing
    getTile 'k' = Floor (Just (Key 1))
    getTile 'd' = Door 1
    getTile 'c' = Floor (Just Coin)

    getAll :: [[Char]] -> [[Tile]]
    getAll [] = []
    getAll (row:other) = ((getRow row) : (getAll other))

    getRow :: [Char] -> [Tile]
    getRow [] = []
    getRow (it:other) = (getTile it) : (getRow other)

main :: IO ()
main = drawingOf (scaled 2 2 (translated (-4) 4 (renderTileGrid myTileGrid)))

-- | Homework 3
drawSnowflake :: Double -> Int -> Picture
drawSnowflake a x = (rotated (2*pi*0/3) (translated 0 (a*(sqrt 3)/6) (drawFragment a x)))
                 <> (rotated (2*pi*1/3) (translated 0 (a*(sqrt 3)/6) (drawFragment a x)))
                 <> (rotated (2*pi*2/3) (translated 0 (a*(sqrt 3)/6) (drawFragment a x)))
  where
    drawFragment :: Double -> Int -> Picture
    drawFragment a 0 = (polygon [(-a/2, 0), (a/2, 0)])
    drawFragment a x = (translated (-a/3) 0 (drawFragment (a/3) (x-1)))
                    <> (translated (-a/12) (a*(sqrt 3)/12) (rotated (pi/3) (drawFragment (a/3) (x-1))))
                    <> (translated (a/12) (a*(sqrt 3)/12) (rotated (-pi/3) (drawFragment (a/3) (x-1))))
                    <> (translated (a/3) 0 (drawFragment (a/3) (x-1)))

-- main :: IO ()
-- main = drawingOf (drawSnowflake 10 3)
