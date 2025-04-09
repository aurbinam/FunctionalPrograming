{-# LANGUAGE LambdaCase #-}
import Data.List (groupBy, sortOn, partition, sortBy, find)
import Data.Ord (comparing)
import Control.Monad (forever)
import Data.Char (ord, toUpper, isAlpha)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Text.Printf (printf)

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

-- Detect if the spreadsheet has cyclic references
isCyclic :: Spreadsheet -> Bool
isCyclic sheet = not (null (findCycles sheet))

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

-- Function to convert a row of cells into a string
showRow :: [(Position, CellValue)] -> String
showRow row = unwords [positionToReference pos ++ "=" ++ maybe "?" show (evalCell pos row) | (pos, _) <- row]


-- Function to print the spreadsheet as a table
printSpreadsheet :: Spreadsheet -> IO ()
printSpreadsheet sheet = do
    let evaluated = Map.fromList [((r, c), maybe "?" show (evalCell (r, c) sheet)) | ((r, c), _) <- sheet]
        rows = map fst (Map.keys evaluated)
        cols = map snd (Map.keys evaluated)
        maxRow = if null rows then 0 else maximum rows
        maxCol = if null cols then 0 else maximum cols

    -- Print column headers
    putStr "    " -- offset for row labels
    mapM_ (\c -> printf "%-8s" (indexToColumn c)) [1..maxCol]
    putStrLn ""

    -- Print each row with values
    mapM_ (\r -> do
        printf "%-4d" r
        mapM_ (\c -> printf "%-8s" (Map.findWithDefault "?" (r, c) evaluated)) [1..maxCol]
        putStrLn ""
        ) [1..maxRow]


--Exercises of the handout:

-- 1. Evaluates a cell's value
evalCell :: Position -> Spreadsheet -> Maybe Double  
evalCell pos sheet = case lookup pos sheet of
    Just (Number x) -> Just x
    Just (Formula f) -> Just (f sheet)
    Just (Reference ref) -> 
        case parseReference ref >>= (`lookup` sheet) of
            Just (Number x) -> Just x
            Just (Formula f) -> Just (f sheet)
            Just (Reference newRef) -> case parseReference newRef of 
                Just newPos -> evalCell newPos sheet
                Nothing -> Nothing

--evaluate the cell but without the references
--evalCell :: Position -> Spreadsheet -> Maybe Double  
--evalCell pos sheet = case lookup pos sheet of
    --Just (Number x) -> Just x
    --Just (Formula f) -> Just (f sheet)
    --Nothing -> Nothing

-- 2. Updates a cell's value
updateCell :: Position -> CellValue -> Spreadsheet -> Spreadsheet
updateCell pos val sheet = (pos, val) : filter ((/= pos) . fst) sheet

-- 3. Maps a function to all cells
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f = map (\(pos, val) -> (pos, f val))

-- 4. Filters cells by value
filterCellsByValue :: (Double -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue pred = filter (\(_, val) -> case val of
    Number x -> pred x
    Formula _ -> False)

-- 5. Counts cells by value
countCellsBy :: (Double -> Bool) -> Spreadsheet -> Int
countCellsBy pred = length . filterCellsByValue pred

-- 6. Sums values in a given range (inclusive)
sumRange :: Position -> Position -> Spreadsheet -> Double
sumRange (r1, c1) (r2, c2) sheet =
    sum [ val | ((r, c), _) <- sheet, r >= r1, r <= r2, c >= c1, c <= c2
              , Just val <- [evalCell (r, c) sheet] ]


-- 7. Applies a numeric function to all cells in a given range
mapRange :: (Double -> Double) -> Position -> Position -> Spreadsheet -> Spreadsheet
mapRange f (r1, c1) (r2, c2) sheet =
    [if r1 <= r && r <= r2 && c1 <= c && c <= c2 then (pos, apply val) else (pos, val) | (pos@(r, c), val) <- sheet]
  where apply (Number x) = Number (f x)
        apply f@(Formula _) = f

-- 8. Sorts spreadsheet by numeric values, ignoring formulas
sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue sheet =
    let -- Extract only numeric/evaluable values with original positions
        cellsWithValues = [((r, c), val) | ((r, c), val) <- sheet, evalCell (r, c) sheet /= Nothing]
        values = map (\(pos, _) -> evalCell pos sheet) cellsWithValues
        sortedValues = map Number $ sortOn id (mapMaybe id values)
        updatedCells = zip (map fst cellsWithValues) sortedValues

        -- Replace only the numeric cells with new values, keep others untouched
        resultSheet = map (\(pos, val) ->
            case lookup pos updatedCells of
                Just newVal -> (pos, newVal)
                Nothing     -> (pos, val)) sheet
    in resultSheet
    

-- Menu for the user to interact and touch the spreadsheet
menu :: Spreadsheet -> IO ()
menu sheet = do
    putStrLn "\nOptions:"
    putStrLn "1. Print Spreadsheet"
    putStrLn "2. Evaluate a cell"
    putStrLn "3. Update a Cell"
    putStrLn "4. Apply a function to all the cells in the spreadsheet"
    putStrLn "5. Filter cells by value"
    putStrLn "6. Count Cells based on a predicate"
    putStrLn "7. Sum a range of cells"
    putStrLn "8. Apply a function to a range of cells"
    putStrLn "9. Sort cells by value"
    putStrLn "10. Pass cell reference to cell coordinate"
    putStrLn "11. Pass a cell coordinate to cell reference"
    putStrLn "12. Exit"
    putStr "Choose an option: "
    choice <- getLine
    case choice of
        "1" -> do
            printSpreadsheet sheet
            menu sheet
        "2" -> do
            putStr "Enter cell (e.g., A1 or (1,2)): "
            cell <- getLine
            let cellRef = if head cell == '(' then
                            case readMaybe cell of
                                Just (r, c) -> Just (positionToReference (r, c))  
                                Nothing -> Nothing
                        else Just cell
    
            case cellRef of
                Just ref -> case parseReference ref of
                    Just (row, col) -> case evalCell (row, col) sheet of
                        Just val -> putStrLn $ "Value: " ++ show val
                        Nothing  -> putStrLn "Cell not found."
                    Nothing -> putStrLn "Invalid cell reference."
                Nothing -> putStrLn "Invalid cell format."
            printSpreadsheet sheet
            menu sheet

        --This is the one with the cells just as numbers
        --"2" -> do
            --putStr "Enter row: "
            --row <- readLn
            --putStr "Enter column: "
            --col <- readLn
            --case evalCell (row, col) sheet of
                --Just val -> putStrLn $ "Value: " ++ show val
                --Nothing  -> putStrLn "Cell not found."
            --menu sheet

        "3" -> do
            putStr "Enter cell (e.g., A1 or (1,2)): "
            cell <- getLine
            let cellRef = if head cell == '(' then
                            case readMaybe cell of
                                Just (r, c) -> Just (r, c)  
                                Nothing -> Nothing
                        else parseReference cell 

            case cellRef of
                Just (row, col) -> do
                    putStr "Enter value (number only for now): "
                    val <- readLn
                    let newSheet = updateCell (row, col) (Number val) sheet
                    putStrLn "Cell updated."
                    menu newSheet
                Nothing -> do
                    putStrLn "Invalid cell reference."
                    printSpreadsheet sheet
                    menu sheet
  
        --"3" -> do
            --putStr "Enter row: "
            --row <- readLn
            --putStr "Enter column: "
            --col <- readLn
            --putStr "Enter value (number only for now): "
            --val <- readLn
            --let newSheet = updateCell (row, col) (Number val) sheet
            --putStrLn "Cell updated."
            --menu newSheet
        "4" -> do
            putStrLn "Choose a function to apply to all numeric cells:"
            putStrLn "1. Multiply by a constant"
            putStrLn "2. Add a constant"
            putStrLn "3. Subtract a constant"
            putStrLn "4. Divide by a constant"
            putStr "Enter choice: "
            funcChoice <- getLine

            putStr "Enter the constant: "
            constant <- readLn :: IO Double

            let func = case funcChoice of
                    "1" -> (* constant)
                    "2" -> (+ constant)
                    "3" -> subtract constant
                    "4" -> (/ constant)
                    _   -> id 

            let newSheet = mapSpreadsheet (\val -> case val of 
                                Number x -> Number (func x) 
                                f@(Formula _) -> f) sheet
                        
            putStrLn "Function applied to all numeric cells."
            printSpreadsheet newSheet
            menu newSheet  

        "5" -> do
            putStrLn "Enter condition type:"
            putStrLn "1. Greater than (>)"
            putStrLn "2. Less than (<)"
            putStrLn "3. Equal to (==)"
            putStr "Enter choice: "
            condChoice <- getLine

            putStr "Enter threshold value: "
            threshold <- readLn :: IO Double

            let cond = case condChoice of
                    "1" -> (> threshold)
                    "2" -> (< threshold)
                    "3" -> (== threshold)
                    _   -> const False  

            let filteredSheet = filterCellsByValue cond sheet
            putStrLn "Filtered Spreadsheet:"
            printSpreadsheet filteredSheet
            menu sheet
        "6" -> do
            putStrLn "Enter condition type:"
            putStrLn "1. Greater than (>)"
            putStrLn "2. Less than (<)"
            putStrLn "3. Equal to (==)"
            putStr "Enter choice: "
            condChoice <- getLine

            putStr "Enter threshold value: "
            threshold <- readLn :: IO Double

            let cond = case condChoice of
                    "1" -> (> threshold)
                    "2" -> (< threshold)
                    "3" -> (== threshold)
                    _   -> const False  

            let count = countCellsBy cond sheet
            putStrLn $ "Count of matching cells: " ++ show count
            menu sheet
        "7" -> do
            putStr "Enter start cell (e.g., A1 or (1,2)): "
            startCell <- getLine
            putStr "Enter end cell (e.g., B3 or (3,2)): "
            endCell <- getLine
            let startRef = if head startCell == '(' then readMaybe startCell else parseReference startCell
            let endRef   = if head endCell == '(' then readMaybe endCell else parseReference endCell
            case (startRef, endRef) of
                (Just (r1, c1), Just (r2, c2)) -> 
                    putStrLn $ "Sum of range: " ++ show (sumRange (r1, c1) (r2, c2) sheet)
                _ -> putStrLn "Invalid cell references."
            menu sheet

        "8" -> do
            putStr "Enter start cell (e.g., A1 or (1,2)): "
            startCell <- getLine
            putStr "Enter end cell (e.g., B3 or (3,2)): "
            endCell <- getLine
            let startRef = if head startCell == '(' then readMaybe startCell else parseReference startCell
            let endRef   = if head endCell == '(' then readMaybe endCell else parseReference endCell
            case (startRef, endRef) of
                (Just (r1, c1), Just (r2, c2)) -> do
                    putStrLn "1. Multiply by a constant"
                    putStrLn "2. Add a constant"
                    putStrLn "3. Subtract a constant"
                    putStrLn "4. Divide by a constant"
                    putStr "Enter choice: "
                    funcChoice <- getLine

                    putStr "Enter the constant: "
                    factor <- readLn :: IO Double
                    let func = case funcChoice of
                            "1" -> (* factor)
                            "2" -> (+ factor)
                            "3" -> subtract factor
                            "4" -> (/ factor)
                            _   -> id
                    let newSheet = mapRange (func) (r1, c1) (r2, c2) sheet
                    putStrLn "Function applied to range."
                    printSpreadsheet newSheet
                    menu newSheet
                _ -> do
                    putStrLn "Invalid cell references."
                    menu sheet

        "9" -> do
            let newSheet = sortCellsByValue sheet
            putStrLn "Cells sorted by value."
            printSpreadsheet newSheet
            menu newSheet

        "10" -> do
            putStrLn "Enter cell reference: "
            cellRef <- getLine
            print $ parseReference cellRef
            menu sheet
        
        "11" -> do
            putStrLn "Enter cell coordinate: "
            cellCoord <- getLine
            print $ positionToReference (read cellCoord)
            menu sheet

        "12" -> do
            putStrLn "Goodbye!, thank you for using this program"

        _   -> do
            putStrLn "Invalid option. Try again."
            menu sheet

-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to the Spreadsheet Program!"
    let initialSheet =  [((1,1), Number 576), ((1,2), Formula (\_ -> 20)), 
                        ((2,1), Number 30), ((2,2), Number 40)]
    print $ findCycles initialSheet
    menu initialSheet
