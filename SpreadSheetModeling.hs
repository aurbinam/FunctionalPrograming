{-# LANGUAGE LambdaCase #-}
import Data.Char (ord, toUpper, isAlpha)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (sortOn)

-- Types of spreadsheet cells
data CellValue = Number Double
               | Formula (Spreadsheet -> Double)
               | Reference String 

type Position = (Int, Int)
type Spreadsheet = [(Position, CellValue)]

instance Show CellValue where
    show (Number x)  = show x
    show (Formula _) = "Formula"
    show (Reference ref) = ref

-- Convert column reference to column index
columnToIndex :: String -> Int
columnToIndex = foldl (\acc c -> acc * 26 + (ord (toUpper c) - ord 'A' + 1)) 0

-- Convert column index to column reference
indexToColumn :: Int -> String
indexToColumn 0 = ""
indexToColumn n = indexToColumn ((n - 1) `div` 26) ++ [toEnum (ord 'A' + (n - 1) `mod` 26)]

-- Parse reference (e.g., "B3" -> (3, 2))
parseReference :: String -> Maybe Position
parseReference ref = 
    let (colPart, rowPart) = span isAlpha ref
    in case reads rowPart of
        [(row, "")] -> Just (row, columnToIndex colPart)
        _ -> Nothing

-- Convert Position to string reference
positionToReference :: Position -> String
positionToReference (row, col) = indexToColumn col ++ show row

-- Detect cycles in references
findCycles :: Spreadsheet -> [[Position]]
findCycles sheet = mapMaybe findCycle sheet
  where
    findCycle (pos, Reference ref) = detectCycle pos ref []
    findCycle _ = Nothing
    detectCycle startRef ref visited = 
        case parseReference ref of
            Just newPos | newPos == startRef -> Just (startRef : visited)
                        | newPos `elem` visited -> Just (newPos : visited)
                        | otherwise -> case lookup newPos sheet of
                            Just (Reference newRef) -> detectCycle startRef newRef (newPos : visited)
                            _ -> Nothing
            _ -> Nothing

-- Print the spreadsheet as a table
printSpreadsheet :: Spreadsheet -> IO ()
printSpreadsheet sheet = do
    let evaluated = Map.fromList [((r, c), maybe "?" show (evalCell (r, c) sheet)) | ((r, c), _) <- sheet]
        rows = map fst (Map.keys evaluated)
        cols = map snd (Map.keys evaluated)
        maxRow = if null rows then 0 else maximum rows
        maxCol = if null cols then 0 else maximum cols
    putStr "    "
    mapM_ (\c -> printf "%-8s" (indexToColumn c)) [1..maxCol]
    putStrLn ""
    mapM_ (\r -> do
        printf "%-4d" r
        mapM_ (\c -> printf "%-8s" (Map.findWithDefault "?" (r, c) evaluated)) [1..maxCol]
        putStrLn ""
        ) [1..maxRow]

-- Evaluate a cell's value
evalCell :: Position -> Spreadsheet -> Maybe Double  
evalCell pos sheet = case lookup pos sheet of
    Just (Number x) -> Just x
    Just (Formula f) -> Just (f sheet)
    Just (Reference ref) -> 
        case parseReference ref >>= (`lookup` sheet) of
            Just (Number x) -> Just x
            Just (Formula f) -> Just (f sheet)
            Just (Reference newRef) -> parseReference newRef >>= \newPos -> evalCell newPos sheet
            _ -> Nothing
    Nothing -> Nothing

-- Update a cell's value
updateCell :: Position -> CellValue -> Spreadsheet -> Spreadsheet
updateCell pos val sheet = (pos, val) : filter ((/= pos) . fst) sheet

-- Map a function to all cells
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f = map (\(pos, val) -> (pos, f val))

-- Filter cells by numeric value
filterCellsByValue :: (Double -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue pred = filter (\(_, val) -> case val of
    Number x -> pred x
    _ -> False)

-- Count cells matching a condition
countCellsBy :: (Double -> Bool) -> Spreadsheet -> Int
countCellsBy pred = length . filterCellsByValue pred

-- Sum values in a given range
sumRange :: Position -> Position -> Spreadsheet -> Double
sumRange (r1, c1) (r2, c2) sheet =
    sum [ val | ((r, c), _) <- sheet, r >= r1, r <= r2, c >= c1, c <= c2
              , Just val <- [evalCell (r, c) sheet] ]

-- Apply function to a range
mapRange :: (Double -> Double) -> Position -> Position -> Spreadsheet -> Spreadsheet
mapRange f (r1, c1) (r2, c2) sheet =
    [if r1 <= r && r <= r2 && c1 <= c && c <= c2 then (pos, apply val) else (pos, val) | (pos@(r, c), val) <- sheet]
  where apply (Number x) = Number (f x)
        apply f@(Formula _) = f

-- Sort cells by numeric value
sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue sheet =
    let cellsWithValues = [((r, c), val) | ((r, c), val) <- sheet, evalCell (r, c) sheet /= Nothing]
        values = map (\(pos, _) -> evalCell pos sheet) cellsWithValues
        sortedValues = map Number $ sortOn id (mapMaybe id values)
        updatedCells = zip (map fst cellsWithValues) sortedValues
    in map (\(pos, val) -> maybe (pos, val) (\newVal -> (pos, newVal)) (lookup pos updatedCells)) sheet

-- Main test function
main :: IO ()
main = do
    putStrLn "Welcome to the Spreadsheet Program!"
    let initialSheet = [((1,1), Number 576), ((1,2), Formula (\_ -> 20)), 
                        ((2,1), Number 30), ((2,2), Number 40)]

    putStrLn "\n1. Initial Spreadsheet:"
    printSpreadsheet initialSheet

    putStrLn "\n2. Detect Cycles:"
    print $ findCycles initialSheet

    putStrLn "\n3. Evaluate cell (1,1):"
    print $ evalCell (1,1) initialSheet

    putStrLn "\n4. Update cell (1,2) to 30:"
    let sheet1 = updateCell (1,2) (Number 30) initialSheet
    printSpreadsheet sheet1

    putStrLn "\n5. Apply (*2) to all numeric cells:"
    let sheet2 = mapSpreadsheet (\val -> case val of 
                                Number x -> Number (x * 2)
                                f@(Formula _) -> f) sheet1
    printSpreadsheet sheet2

    putStrLn "\n6. Filter cells with value > 30:"
    printSpreadsheet $ filterCellsByValue (>30) sheet2

    putStrLn "\n7. Count cells with value > 30:"
    print $ countCellsBy (>30) sheet2

    putStrLn "\n8. Sum of range (1,1) to (2,2):"
    print $ sumRange (1,1) (2,2) sheet2

    putStrLn "\n9. Apply (*2) to range (1,1) to (2,2):"
    let sheet3 = mapRange (*2) (1,1) (2,2) sheet2
    printSpreadsheet sheet3

    putStrLn "\n10. Sort cells by value:"
    let sortedSheet = sortCellsByValue sheet2
    printSpreadsheet sortedSheet

    putStrLn "\n11. Convert reference \"A1\" to position:"
    print $ parseReference "A1"

    putStrLn "\n12. Convert position (1,1) to reference:"
    print $ positionToReference (1,1)
    
--PS C:\Users\adaau\Documents\College\2º Year\Functional Programming\FunctionalPrograming> GHCi SpreadSheetModeling
--GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
--[1 of 2] Compiling Main             ( SpreadSheetModeling.hs, interpreted )
--Ok, one module loaded.
--ghci> main 
--Welcome to the Spreadsheet Program!
--
--1. Initial Spreadsheet:
--    A       B       
--1   576.0   20.0    
--2   30.0    40.0    
--
--2. Detect Cycles:
--[]
--
--3. Evaluate cell (1,1):
--Just 576.0
--
--4. Update cell (1,2) to 30:
--    A       B       
--1   576.0   30.0    
--2   30.0    40.0    
--
--5. Apply (*2) to all numeric cells:
--    A       B       
--1   1152.0  60.0    
--2   60.0    80.0    
--
--6. Filter cells with value > 30:
--    A       B       
--1   1152.0  60.0
--2   60.0    80.0    
--
--7. Count cells with value > 30:
--4
--
--8. Sum of range (1,1) to (2,2):
--1352.0
--
--9. Apply (*2) to range (1,1) to (2,2):
--    A       B
--1   2304.0  120.0   
--2   120.0   160.0
--
--10. Sort cells by value:
--    A       B
--1   60.0    60.0
--2   80.0    1152.0
--
--11. Convert reference "A1" to position:
--Just (1,1)
--
--12. Convert position (1,1) to reference:
--"A1"
--ghci>
