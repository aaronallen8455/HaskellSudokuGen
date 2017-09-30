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
                 , seed :: Int
                 }

data Grid = Grid { cells :: Array (Int,Int) Cell
                 , size :: Int
                 , sortedBoxes :: Array (Int,Int) [(Int,Int)] 
                 }

type Group = [Cell]


-- the entry point where user enters the grid size and finished grid is printed
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Enter the grid size."
  len <- readLn :: IO Int
  when (len > 2) $ do
    gen <- newStdGen
    let grid = initGrid len gen
        vals = take (len^2) values
        initBox = map (cells grid !) $ sortedBoxes grid ! (0,0)
        finishedGrid = fromJust $ find grid initBox vals
    printGrid finishedGrid
    main

-- see if the value can be assigned to the given cell
canAssign :: Grid -> Cell -> Char -> Bool
canAssign (Grid cells size _) (Cell (cr,cc) cvalue _) v
  | cvalue /= 'x' = False
  | otherwise =
    let rowStart = cr `div` size * size
        colStart = cc `div` size * size
        row = S.fromList . map (value . (cells !)) $ zip [0..size ^ 2 - 1] (repeat cc)
        col = S.fromList . map (value . (cells !)) $ zip (repeat cr) [0..size ^ 2 -1]
        box = S.fromList . map (value . (cells !)) $  [(br,bc) | br <- [rowStart..rowStart + size-1], bc <- [colStart..colStart + size-1], br /= cr, bc /= cc]
    in not $ S.member v row && S.member v col && S.member v box


-- list of possible values
values ="123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&<>*()-+={}[]\\/?"

-- Building the starting grid
initGrid :: Int -> StdGen -> Grid
initGrid size gen =
  let dim = size ^ 2
      cells = listArray ((0,0),(dim-1,dim-1)) $ zipWith ($)  [Cell (r,c) 'x' | r <- [0..dim - 1], c <- [0..dim - 1]] (randoms gen)
      sortedBoxes = array ((0,0),(size-1,size-1)) $ getSortedBoxes cells size
  in Grid cells size sortedBoxes

getBox size r c = do
  cr <- map (+ (size * r)) [0..size-1]
  cc <- map (+ (size * c)) [0..size-1]
  return (cr,cc)

getSortedBoxes cells size = do
  bc <- [0..size-1]
  br <- [0..size-1]
  return ((br,bc), map coords . sortBy (compare `on` seed) . map (cells !) $ getBox size br bc)


-- pretty print the grid
printGrid :: Grid -> IO ()
printGrid (Grid arr size _) = 
  putStr . unlines . map (intersperse ' ' . map value) . groupBy (\a b -> (fst . coords) a == (fst . coords) b) . elems $ arr

-- change the value at the given coordinates if possible.
assignCell :: Grid -> Cell -> Char -> Maybe Grid
assignCell g@(Grid arr size boxes) c@(Cell coords _ seed) val =
  if canAssign g c val
  then Just $ Grid (arr // [(coords, Cell coords val seed)]) size boxes
  else Nothing


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
subFind grid@(Grid cells size boxes) c@(Cell (row,col) _ _) values@(v:vals) = do
  newGrid <- assignCell grid c v
  let bound = size * (size - 1)
  if col < bound || row < bound
  then find newGrid (getNextSortedBox grid c) values
  else find newGrid (map (cells !) $ boxes ! (0,0)) vals
  

getNextSortedBox (Grid cells size sortedBoxes) (Cell (cr,cc) _ _) =
  let c = div cc size + 1
      r = div cr size
  in if c == size 
  then map (cells !) $ sortedBoxes ! (r+1,0)
  else map (cells !) $ sortedBoxes ! (r,c)
  
  




