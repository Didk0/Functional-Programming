import qualified Data.Map as Map

class Truthy a where
    truthy :: a -> Bool

instance Truthy Int where
    truthy 0 = False
    truthy _ = True

-- instance Truthy Bool where
--     truthy True = True 
--     truthy False = False

instance Truthy Bool where
    truthy = id

instance Truthy [a] where
    truthy [] = False
    truthy _ = True 

instance Truthy (Maybe a) where
    truthy Nothing = False 
    truthy _ = True

ifTruthy :: (Truthy a) => a -> b -> b -> b
ifTruthy testValue yesValue noValue = if truthy testValue then yesValue else noValue

type FactultyNumber = Int 
type Name = String 
type Grade = Double
type StudentInfo = (Name, Grade)
type Student = (FactultyNumber, (Name, Grade))
type Class = [Student]

students :: Class
students = [
    (41111, ("Yordan", 5.75)),
    (42222, ("Ivana", 5.35)),
    (43333, ("Antoniya", 4.65)),
    (44444, ("Atanas", 3.25)),
    (45555, ("Stefan", 4.11)),
    (46666, ("Sofiya", 5.05))
    ]

find :: FactultyNumber -> Class -> Maybe StudentInfo
find _ [] = Nothing 
find num ((fn, info):xs) = if num == fn then Just info else find num xs

getFns :: Class -> [FactultyNumber]
getFns = map fst

aboveAverage :: Class -> [Name]
aboveAverage students = [name | (_, (name, grade)) <- students, grade > average] where
    average = sum [grade | (_, (_, grade)) <- students] / fromIntegral (length students)

aboveRest :: Class -> [(Name, Double)]
aboveRest students = [(name, studentAverage student) | student@(_, (name, grade)) <- students] where
    studentAverage student = sum [grade | (_, (_, grade)) <- [s | s <- students, s /= student]] / fromIntegral (length students)

type ClassMap = Map.Map FactultyNumber StudentInfo

studentsMap :: Map.Map FactultyNumber StudentInfo
studentsMap = Map.fromList students

findStudent :: FactultyNumber -> ClassMap -> Maybe StudentInfo
findStudent = Map.lookup

scholarship :: ClassMap -> [Name]
scholarship = Map.elems . Map.map fst . Map.filter (\(_, grade) -> grade >= 5.5)