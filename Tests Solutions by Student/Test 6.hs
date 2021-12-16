type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade

data Result a
  = Success a
  | Failure String

-- 1

studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA ((Student name A):xs) = name : studentsWithA(xs)
studentsWithA ((Student name grade):xs) = studentsWithA(xs)

example1 = studentsWithA [Student "Jack" D, Student "Jane" A]

main = print example1

-- 2

combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Success a) (Success b) = (Success (f a b))
combineResultsWith f (Failure a) b = (Failure a)
combineResultsWith f (Success a) (Failure b) = (Failure b)

example2 = combineResultsWith (+) (Success 2) (Success 3)
example3 = combineResultsWith (+) (Failure "x is undefined") (Failure "crash")

