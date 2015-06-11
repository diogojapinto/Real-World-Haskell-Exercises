import Data.List

length' :: [a] -> Int
length' (x:xs) = 1 + (length' xs)
length' [] = 0

sum' :: Num a => [a] -> a
sum' (x:xs) = x + (sum' xs)
sum' [] = 0

average :: Fractional a => [a] -> a
average xs = (sum' xs) / (fromIntegral $ length' xs)

palindromize :: [a] -> [a]
palindromize [] = []
palindromize (x:xs) = [x] ++ palindromize xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = let l = length' xs
                  in if head xs == last xs
                     then isPalindrome (tail . init $ xs)
                     else False

lengthComp :: [a] -> [a] -> Ordering
lengthComp x y
    | length' x >= length' y = GT
    | otherwise              = LT

sortBySublistLen :: Ord a => [[a]] -> [[a]]
sortBySublistLen xs = sortBy lengthComp xs

intersperse' :: Char -> [String] -> String
intersperse' _ [] = ""
intersperse' _ [x] = x
intersperse' c (x:xs) = x ++ [c] ++ intersperse' c xs


data Tree a = Node a (Tree a) (Tree a)
              | Empty
              deriving (Show)

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Node _ ls rs) = 1 + max (treeHeight ls) (treeHeight rs)


data Direction = TurnLeft | TurnRight | Straight
                 deriving (Show)

getDir :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Direction
getDir (a,b) (c,d) (e,f)
    | slope2 > slope1 = TurnLeft
    | slope2 < slope1 = TurnRight
    | otherwise       = Straight
    where slope1 = (d-b)/(c-a)
          slope2 = (f-d)/(e-c)

getTriplesDirs :: [(Float, Float)] -> [Direction]
getTriplesDirs ls@(a:b:c:xs) = (getDir a b c):(getTriplesDirs $ tail ls)
getTriplesDirs _ = []


convexHull :: [(Float, Float)] -> [(Float, Float)]
convexHull xs = 
    let 
        -- get the lowest y-coord valued point
        lowestYCoord = min . map snd $ xs
        eligiblePoints = filter (\x -> snd x == lowestYCoord) xs
        lowestXCoord = min . map fst $ eligiblePoints
        x0 = (lowestXCoord, lowestYCoord)
        xN = filter (\x -> x /= x0)

        -- order the rest of the points
        xNSorted = x0 : (sortBy (angleOrd x0) xN)
    in 
        computeConvexHull (x0:xN) (getTriplesDirs )

angleOrd :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Ordering
angleOrd (px, py) (a,b) (c,d)
    | slope1 >= slope2 = GT
    | slope1 < slope2 = LT
    where slope1 = (b-py)/(a-px)
          slope2 = (d-py)/(c-px)