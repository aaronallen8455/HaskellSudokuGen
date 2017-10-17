{-#LANGUAGE BangPatterns#-}

import Data.List (transpose, groupBy, intersperse, sortBy)
import qualified Data.Set as S (fromList, member)
import Data.Array (Array, listArray, (!), elems, (//), assocs, array)
import System.Random (randoms, newStdGen, StdGen, mkStdGen)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (when)
import Data.Function (on)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)
import Control.Concurrent (MVar, newMVar, modifyMVar)
import Control.Concurrent.Async (Async, withAsync, wait)

-- Make types for the cell and grid
data Cell = Cell { coords :: (Int,Int)
                 , value :: Char
                 }

data Grid = Grid { cells :: Array (Int,Int) Cell
                 , size :: Int
                 , sortedRows :: Array Int [(Int,Int)]
                 }

type Group = [Cell]

-- non-blocking semaphore
newtype NBSem = NBSem (MVar Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newMVar i
  return (NBSem m)

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
  modifyMVar m $ \i ->
    if i == 0
    then return (i, False)
    else let !z = i-1 in return (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  modifyMVar m $ \i ->
    let !z = i+1 in return (z, ())

-- the entry point where user enters the grid size and finished grid is printed
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Enter the grid size."
  len <- readLn :: IO Int
  when (len > 1) $ do
    gen <- newStdGen
    let grid = initGrid len gen
        vals = take (len^2) values
        initBox = map (cells grid !) $ sortedRows grid ! 0
    finishedGrid <- fromJust <$> find grid initBox vals
    printGrid finishedGrid
    main


-- list of possible values
values ="123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&<>*()-+={}[]\\/?"

-- Building the starting grid
initGrid :: Int -> StdGen -> Grid
initGrid size gen =
  let dim = size ^ 2
      cells = listArray ((0,0),(dim-1,dim-1)) [Cell (r,c) 'x' | r <- [0..dim - 1], c <- [0..dim - 1]]
      sortedRows = listArray (0, dim - 1) $ getSortedRows size gen
  in Grid cells size sortedRows


-- creates of list of shuffled indexes for each row
getSortedRows :: Int -> StdGen -> [[(Int,Int)]]
getSortedRows size gen = do
  let randNums = randoms gen :: [Int]
      bound = size ^ 2 - 1
  rowIndex <- [0..bound]
  let colIndexes = [0..bound]
  return . zip (repeat rowIndex) . map snd . sortBy (compare `on` fst) . zip (take (size ^ 2) . drop (size ^ 2 * rowIndex) $ randNums) $ colIndexes


-- pretty print the grid
printGrid :: Grid -> IO ()
printGrid (Grid arr size _) =
  putStr . unlines . map (intersperse ' ' . map value) . groupBy (\a b -> (fst . coords) a == (fst . coords) b) . elems $ arr

-- change the value at the given coordinates if possible.
assignCell :: Grid -> Cell -> Char -> Maybe Grid
assignCell g@(Grid arr size rows) c@(Cell coords _) val =
  if canAssign g c val
  then Just $ Grid (arr // [(coords, Cell coords val)]) size rows
  else Nothing


-- see if the value can be assigned to the given cell
canAssign :: Grid -> Cell -> Char -> Bool
canAssign (Grid cells size _) (Cell (cr,cc) cvalue) v
  | cvalue /= 'x' = False
  | otherwise =
    let rowStart = cr `div` size * size
        colStart = cc `div` size * size
        row = S.fromList . map (value . (cells !)) $ zip (repeat cr) [0..size ^ 2 - 1]
        col = S.fromList . map (value . (cells !)) $ zip [0..size ^ 2 -1] (repeat cc)
        box = S.fromList . map (value . (cells !)) $  [(br,bc) | br <- [rowStart..rowStart + size-1], bc <- [colStart..colStart + size-1], br /= cr, bc /= cc]
    in not $ S.member v row || S.member v col || S.member v box


-- iterate over the given group with the subFind function which tries to assign the current value to each cell in a given row
find :: Grid -> Group -> [Char] -> IO (Maybe Grid)
find grid _ [] = return (Just grid)
find _ [] _ = return Nothing
find grid group values =
  foldr (subFind grid values) dowait group []
 where
   dowait as = loop as

   loop [] = return Nothing
   loop (a:as) = do
     r <- wait a
     case r of
       Nothing -> loop as
       g@(Just _) -> return g

-- each fold of subFind will add an async to the list and call inner on it (which will be next subFind)
-- then all the asyncs end up in dowait

-- Tries to assign the current value to a given cell. If it succeeds, we either move to the next row or the next value at top row.
-- If it fails, we fall back to the find function which will try the next cell.
subFind :: Grid -> [Char] -> Cell -> ([Async (Maybe Grid)] -> IO (Maybe Grid)) -> [Async (Maybe Grid)] -> IO (Maybe Grid)
subFind grid@(Grid cells size rows) values@(v:vals) c@(Cell (row,col) _) inner asyncs = do
  case assignCell grid c v of
    Nothing -> inner asyncs
    Just g ->
      if row < size ^ 2 - 1
      then withAsync (find g (map (cells !) $ rows ! (row + 1)) values) $ \a -> inner (a:asyncs)
      else withAsync (find g (map (cells !) $ rows ! 0) vals) $ \a -> inner (a:asyncs)
