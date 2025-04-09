import Data.Maybe

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