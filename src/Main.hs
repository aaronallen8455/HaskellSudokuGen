import Data.List (transpose, groupBy, intersperse, sortBy)
import Data.Array (Array, listArray, (!), elems, (//), assocs)
import System.Random (randoms, newStdGen, StdGen, mkStdGen)
import Data.Maybe (fromJust)
import Control.Monad (when)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.IORef


-- Make types for the cell and grid
data Cell = Cell { coords :: (Int,Int)
                 , value :: Char
                 , seed :: Int
                 }

data Grid = Grid { cells :: Array (Int,Int) Cell, size :: Int }

type Group = [Cell]

-- non-blocking semaphore to throttle thread creation
newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newIORef i
  return $ NBSem m

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    if i == 0
    then (i, False)
    else let !z = i-1 in (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    let !z = z+1 in (z, ())

-- the entry point where user enters the grid size and finished grid is printed
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Enter grid size"
  len <- readLn :: IO Int
  when (len >= 2) $ do
    gen <- newStdGen
    let grid = initGrid len gen
        vals = take (len^2) values
        initBox = fromJust $ getSortedBox grid (0,0)
        --finishedGrid = fromJust $ tryCells grid initBox vals
    sem <- newNBSem 2
    finishedGrid <- findSync sem grid initBox vals
    case finishedGrid of
      Nothing -> putStrLn "Failed"
      Just r -> printGrid r
    main

-- functions for accessing cell groups from a grid
getRow :: Grid -> Int -> Group
getRow (Grid cells size) row = 
  let coords = [(row,col) | col <- [0.. size ^ 2 - 1]]
  in map (cells !) coords

getCol :: Grid -> Int -> Group
getCol (Grid cells size) col =
  let coords = [(row,col) | row <- [0..size ^ 2 - 1]]
  in map (cells !) coords

getBox :: Grid -> (Int,Int) -> Group
getBox (Grid cells size) (row,col) =
  let rowStart = row `div` size * size
      colStart = col `div` size * size
      coords = [(r,c) | r <- [rowStart..rowStart+size-1], c <- [colStart..colStart+size-1]]
  in map (cells !) coords

-- list of possible values
values = ['1'..'9'] ++ ['A'..'Z']

-- Building the starting grid
initGrid :: Int -> StdGen -> Grid
initGrid size gen =
  let dim = size ^ 2
      cells = listArray ((0,0),(dim-1,dim-1)) $ zipWith ($)  [Cell (r,c) 'x' | r <- [0..dim - 1], c <- [0..dim - 1]] (randoms gen)
  in Grid cells size

-- pretty print the grid
printGrid :: Grid -> IO ()
printGrid (Grid arr size) = 
  putStr . unlines . map (intersperse ' ' . map value) . groupBy (\a b -> (fst . coords) a == (fst . coords) b) . elems $ arr

-- change the value at the given coordinates if possible.
assignCell :: Grid -> Cell -> Char -> IO (Maybe Grid)
assignCell grid@(Grid arr size) (Cell coords _ seed) val =
  if canAssign grid coords val
  then return . Just $ Grid (arr // [(coords, Cell coords val seed)]) size
  else return Nothing

-- check if value can be assigned at given coords
canAssign :: Grid -> (Int,Int) -> Char -> Bool
canAssign grid (row,col) val =
  if value (cells grid ! (row,col)) /= 'x'
  then False -- cell already has a value
  else
    let colG = getCol grid col
        rowG = getRow grid row
        boxG = getBox grid (row,col)
    in not . hasVal . map value . concat $ [colG,rowG,boxG]
  where
    hasVal [] = False
    hasVal (v:vs)
      | v == val = True
      | otherwise = hasVal vs


getNextBox :: Grid -> Cell -> IO (Maybe Group)
getNextBox grid cell =
  let coords' = getCoordsForNextBox grid $ coords cell
  in return $ getSortedBox grid coords'

-- sort the cells in a box sorted by their seed number
getSortedBox :: Grid -> (Int,Int) -> Maybe Group
getSortedBox grid (row,col)
  | row > len = Nothing -- coords are outside the grid
  | otherwise = Just . sortBy (\a b -> compare (seed a) (seed b)) . filter (\x -> value x == 'x') $ getBox grid (row,col)
  where len = size grid ^ 2 - 1

-- get the coordinates for the next box in the grid based on old box's coords
getCoordsForNextBox :: Grid -> (Int,Int) -> (Int,Int)
getCoordsForNextBox (Grid _ size) (row,col)
  | col + size > len = (row + size,0)
  | otherwise = (row,col + size)
  where len = size ^ 2 - 1



findSync :: NBSem -> Grid -> Group -> [Char] -> IO (Maybe Grid)
findSync _ grid _ [] = return (Just grid)
findSync _ _ [] _ = return Nothing
findSync sem grid (c:cs) values@(v:vals) = do
  a <- assignCell grid c v
  case a of
    Nothing -> find sem grid cs values -- switch to async
    Just grid' -> do
      nb <- getNextBox grid' c
      case nb of
        Nothing -> do
          -- go to next character, use async
          result <- find sem grid' (fromJust $ getSortedBox grid' (0,0)) vals
          case result of
            Nothing -> find sem grid cs values -- switch to async
            Just _ -> return result
        Just nextBox -> do
          -- go to next box
          result <- findSync sem grid' nextBox values
          case result of
            Nothing -> find sem grid cs values -- switch to async
            Just _ -> return result



-- search for a completed grid using threading
find :: NBSem -> Grid -> Group -> [Char] -> IO (Maybe Grid)
find _ grid _ [] = return (Just grid) -- complete grid
find _ _ [] _ = return Nothing
find sem grid group vals = do
  foldr (subFind sem grid vals) dowait group []
 where
   dowait :: [Async (Maybe Grid)] -> IO (Maybe Grid)
   dowait as = loop (reverse as)

   loop [] = return Nothing
   loop (a:as) = do
     r <- wait a
     case r of
       Nothing -> loop as
       Just _ -> return r




  -- subFind should take a single cell and val list and search from that point
subFind :: NBSem -> Grid -> [Char] -> Cell -> ([Async (Maybe Grid)] -> IO (Maybe Grid)) -> [Async (Maybe Grid)] -> IO (Maybe Grid)
subFind sem grid (v:vs) c inner asyncs = do
  a <- assignCell grid c v
  case a of
    Nothing -> inner asyncs
    Just grid' -> do
      q <- tryAcquireNBSem sem
      n <- getNextBox grid' c
      case n of
        Nothing ->
          -- start assigning next character in first box
          let nextBox = fromJust $ getSortedBox grid' (0,0)
          in if q
          then do
            -- search asynchronously
            let dofind = find sem grid' nextBox vs `finally` releaseNBSem sem
            withAsync dofind $ \a -> inner (a:asyncs)
          else do
            -- search synchronously
            r <- find sem grid' nextBox vs
            case r of
              Nothing -> inner asyncs
              Just _ -> return r
        Just nextBox ->
          -- go to next box
          if q
          then do
            -- search asynchronously
            let dofind = find sem grid' nextBox (v:vs) `finally` releaseNBSem sem
            withAsync dofind $ \a -> inner (a:asyncs)
          else do
            -- search synchronously
            r <- find sem grid' nextBox (v:vs)
            case r of
              Nothing -> inner asyncs
              Just _ -> return r
