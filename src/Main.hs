import Data.List (transpose, groupBy, intersperse, sortBy)
import qualified Data.Set as S (fromList, member)
import Data.Array (Array, listArray, (!), elems, (//), assocs, array)
import System.Random (randoms, newStdGen, StdGen, mkStdGen)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (when)
import Data.Function (on)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)

-- Make types for the cell and grid
data Cell = Cell { coords :: (Int,Int)
                 , value :: Char
                 }

data Grid = Grid { cells :: Array (Int,Int) Cell
                 , size :: Int
                 , sortedRows :: Array Int [(Int,Int)]
                 }

type Group = [Cell]

-- could make the sorted rows array be a structure where a used cell has it's coords removed

-- the entry point where user enters the grid size and finished grid is printed
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Enter the grid size."
  len <- readLn :: IO Int
  when (len > 2) $ do
    gen <- newStdGen
    let grid = initGrid len gen
        vals = take (len^2) values
        initBox = map (cells grid !) $ sortedRows grid ! 0
        finishedGrid = fromJust $ find grid initBox vals
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
--canAssign :: Grid -> Cell -> Char -> Bool
canAssign (Grid cells size _) (Cell (cr,cc) cvalue) v
  | cvalue /= 'x' = False
  | otherwise =
    let rowStart = cr `div` size * size
        colStart = cc `div` size * size
        row = S.fromList . map (value . (cells !)) $ zip (repeat cr) [0..size ^ 2 - 1]
        col = S.fromList . map (value . (cells !)) $ zip [0..size ^ 2 -1] (repeat cc)
        box = S.fromList . map (value . (cells !)) $  [(br,bc) | br <- [rowStart..rowStart + size-1], bc <- [colStart..colStart + size-1], br /= cr, bc /= cc]
    in not $ S.member v row || S.member v col || S.member v box


find :: Grid -> Group -> [Char] -> Maybe Grid
find grid _ [] = Just grid
find _ [] _ = Nothing
find grid group values = do
  r <- search group
  return r
 where
   search [] = Nothing
   search (c:cs) =
     case subFind grid c values of
       g@(Just _) -> g
       Nothing -> search cs

subFind :: Grid -> Cell -> [Char] -> Maybe Grid
subFind grid@(Grid cells size rows) c@(Cell (row,col) _) values@(v:vals) = do
  newGrid <- assignCell grid c v
  let bound = size ^ 2 - 1
  if row < bound
  then find newGrid (map (cells !) $ rows ! (row + 1)) values
  else find newGrid (map (cells !) $ rows ! 0) vals
