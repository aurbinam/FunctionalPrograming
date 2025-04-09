{-# LANGUAGE LambdaCase #-}
import Data.Char (ord, toUpper, isAlpha)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Text.Printf (printf)
import Data.List (sortOn)

--1 Blackjack
-- Suit, the suit of a card: hearts, spades, diamonds, and clubs.
data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Eq, Show)

-- Rank, the rank of a card: numeric and its value (2-10), jack, queen, king, and ace.
data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Eq, Show)

-- Card, the card itself which has a rank and a suit.
data Card = Card Rank Suit
  deriving (Eq, Show)

-- Hand, the hand of a player which is a list of cards, a type synonym will suffice.
type Hand = [Card]

-- Player, either bank or guest.
data Player = Bank | Guest
  deriving (Eq, Show)

-- Define a function faceCards :: Hand -> Integer that returns the number of face cards in the hand.
faceCards :: Hand -> Integer
faceCards (Card r _ : xs) = if (isInt r) then 0 + faceCards xs else 1 + faceCards xs
faceCards [] = 0

isInt :: Rank -> Bool
isInt (Numeric _) = True
isInt _ = False

-- Define a function value :: Hand -> Integer that calculates the total value of a hand but if the value exceeds 21, turn the hand's aces into 1s instead of 11s.
value :: Hand -> Integer
value hand = cardsValue (notAceValue hand) (aceNumber hand)

notAceValue :: Hand -> Integer
notAceValue (Card (Numeric n) _ : xs) = n + notAceValue xs
notAceValue (Card Ace _ : xs) = notAceValue xs
notAceValue (Card r _ : xs) = 10 + notAceValue xs
notAceValue [] = 0

aceNumber :: Hand -> Integer
aceNumber (Card Ace _ : xs) = 1 + aceNumber xs
aceNumber (Card r _ : xs) = 0 + aceNumber xs
aceNumber [] = 0

cardsValue :: Integer -> Integer -> Integer
cardsValue notAceV aceN = if (aceN > 0) then if (notAceV + 11 + aceN - 1 > 21) then notAceV + aceN else notAceV + 11 + aceN - 1 else notAceV

-- Define a function isBlackjack :: Hand -> Bool that determines whether the hand forms a blackjack. A blackjack is hand with 2 cards that has the value of 21.
isBlackjack :: Hand -> Bool
isBlackjack hand = if (value hand == 21 && length hand == 2) then True else False

-- Define a function gameOver :: Hand -> Bool that checks if the given hand loses (value greater than 21).
gameOver :: Hand -> Bool
gameOver hand = if (value hand > 21) then True else False

-- Define a function winner :: Hand -> Hand -> Player given the guest hand and the bank hand returns the player who won. Tie goes to the bank.
winner :: Hand -> Hand -> Player
winner guest bank = if (value guest > value bank && not (gameOver guest)) then Guest else Bank

-- Define an operator (<+) :: Hand -> Hand -> Hand that places the first hand on top of the other and returns the resulting hand.
(<+) :: Hand -> Hand -> Hand
(<+) guest bank = guest ++ bank

-- Define a function handSuit :: Suit -> Hand that given a suit, returns a hand with all 13 cards of that suit.
handSuit :: Suit -> Hand
handSuit suit = [Card (Numeric n) suit | n <- [2..10]] ++ [Card r suit | r <- [Jack, Queen, King, Ace]]

-- Define a function belongsTo :: Card -> Hand -> Bool that given a card and a hand checks whether the card is in this hand.
belongsTo :: Card -> Hand -> Bool
belongsTo card hand = if (elem card hand) then True else False

-- Define a value fullDeck :: Hand that consists of the 52 card complete deck.
fullDeck :: Hand
fullDeck = [Card (Numeric n) suit | n <- [2..10], suit <- [Hearts, Spades, Diamonds, Clubs]] ++ [Card r suit | r <- [Jack, Queen, King, Ace], suit <- [Hearts, Spades, Diamonds, Clubs]]

-- Define a function draw :: Hand -> Hand -> (Hand, Hand) that given a deck and a hand, draws a card from the deck and returns the remaining deck and the new hand. Throw an error if the deck is empty.
draw :: Hand -> Hand -> (Hand, Hand)
draw [] hand = error "Deck is empty"
draw (d:ds) hand = (ds, hand ++ [d])

-- Define a function playBank :: Hand -> Hand -> Hand that given the deck and the current bank's hand plays a move for the bank. The bank's logic is to draw if the current score is less than 16.
playBank :: Hand -> Hand -> Hand
playBank deck bank = if (value bank < 16) then snd (draw deck bank) else bank












--2. Employee
type EId = String
data Date = Date { day :: Int, month :: Int, year :: Int }
instance Show Date where
    show (Date d m y) = show d ++ "/" ++ show m ++ "/" ++ show y
instance Eq Date where
    (==) (Date d1 m1 y1) (Date d2 m2 y2) = d1 == d2 && m1 == m2 && y1 == y2
instance Ord Date where
    (>) (Date d1 m1 y1) (Date d2 m2 y2) = y1 > y2 || (y1 == y2 && m1 > m2) || (y1 == y2 && m1 == m2 && d1 > d2)
    (<) (Date d1 m1 y1) (Date d2 m2 y2) = y1 < y2 || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 < d2)
    (>=) d1 d2 = d1 > d2 || d1 == d2
    (<=) d1 d2 = d1 < d2 || d1 == d2
data WorkPermit = Permit { number :: String, expiryDate :: Date }
  deriving (Show, Eq)
data Employee = Emp
  {
  empId :: EId,
  joinedOn :: Date,
  permit :: Maybe WorkPermit,
  leftOn :: Maybe Date
  }
  deriving (Show, Eq)

-- A function employeesWithOverlappingPermits :: [Employee] -&gt; [(EId, EId)] that returns a list of unique pairs of employee IDs whose permits overlap. Two permits overlap if: the start date of one permit is before or on the end date of the other, and the end date of one permit is after or on the start date of the other.

employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits [] = []
employeesWithOverlappingPermits emps = concat (employeesWithOverlapping (withPermit emps))

employeesWithOverlapping [] = []
employeesWithOverlapping emps = ((overlappingPermits emps) : employeesWithOverlapping (tail emps))

overlappingPermits :: [Employee] -> [(EId, EId)]
overlappingPermits [] = []
overlappingPermits (em@(Emp _ joinedOn1 (Just permit1) _):emps) = map (\e -> ((empId em), (empId e))) (filter (\(Emp _ joinedOn (Just permit) _) -> joinedOn1 <= expiryDate permit || joinedOn <= expiryDate permit) emps)

withPermit :: [Employee] -> [Employee]
withPermit [] = []
withPermit emps = filter (\(Emp _ _ permit _) -> permit /= Nothing) emps

-- A function employeesByTenure :: [Employee] -&gt; [(Int, [Employee])] that returns a list of tuples where the first element is the tenure (measured in years) and the second is the list of employees. It essentially groups the employees by their tenure.

employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure [] = []
employeesByTenure emps = employeesTupple $ withLeftDate emps

employeesTupple :: [Employee] -> [(Int, [Employee])]
employeesTupple [] = []
employeesTupple emps = (getTenure (head emps), filter (\emp -> getTenure emp == getTenure (head emps)) emps) : employeesTupple (filter (\emp -> getTenure emp /= getTenure (head emps)) emps)

getTenure :: Employee -> Int
getTenure (Emp _ joined _ Nothing) = 0
getTenure (Emp _ joined _ (Just leftOn)) = (year leftOn) - (year joined)

withLeftDate :: [Employee] -> [Employee]
withLeftDate [] = []
withLeftDate emps = filter (\(Emp _ _ _ leftOn) -> leftOn /= Nothing) emps

-- A function longestWorkingEmployee :: [Employee] -&gt; Maybe Employee that returns the employee that has worked the most amount of time.

longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee [] = Nothing
longestWorkingEmployee [emp] = Just emp
longestWorkingEmployee (emp1:emp2:xs) = 
  if joinedOn emp1 <= joinedOn emp2 
    then longestWorkingEmployee (emp1:xs) 
    else longestWorkingEmployee (emp2:xs)

-- A function withExpiredPermit :: [Employee] -&gt; Date -&gt; [EId] that given the current date returns the ids of employees with an expired permit.

withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit [] _ = []
withExpiredPermit emps currentDate = map (\(Emp eId _ _ _) -> eId) (filter (\(Emp _ _ (Just permit) _) -> currentDate > (expiryDate permit)) (withPermit emps))

-- A function avgYearsWorked :: [Employee] -&gt; Double that returns the average years an employee worked in the company. Consider only employees who have left.

sumTenure :: [Employee] -> Int
sumTenure [] = 0
sumTenure (e:es) = (getTenure e) + sumTenure es

avgYearsWorked :: [Employee] -> Double
avgYearsWorked [] = 0
avgYearsWorked emps = (fromIntegral (sumTenure (withLeftDate emps))) / fromIntegral (length (withLeftDate emps))










-- 3. Spreadsheet
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

--PS C:\Users\adaau\Documents\College\2º Year\Functional Programming\FunctionalPrograming> GHCi Group10Haskell     
--GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
--[1 of 2] Compiling Main             ( Group10Haskell.hs, interpreted )
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
