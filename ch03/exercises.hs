import Data.List
import Debug.Trace

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

getDir :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
getDir (a,b) (c,d) (e,f)
    | crossProd > 0   = TurnLeft
    | crossProd < 0   = TurnRight
    | otherwise = Straight
    where (x1,y1) = (c-a,d-b)
          (x2,y2) = (e-c,f-d)
          crossProd = x1*y2 - x2*y1

getTriplesDirs :: [(Double, Double)] -> [Direction]
getTriplesDirs ls@(a:b:c:xs) = (getDir a b c):(getTriplesDirs $ tail ls)
getTriplesDirs _ = []


convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull xs = 
    let 
        -- get the lowest y-coord valued point
        lowestYCoord = minimum $ map snd xs
        eligiblePoints = filter (\x -> snd x == lowestYCoord) xs
        lowestXCoord = minimum $ map fst eligiblePoints
        x0 = (lowestXCoord, lowestYCoord)
        xN = filter (\x -> x /= x0) xs

        -- order the rest of the points
        xsSorted = x0 : (sortBy (angleOrd x0) xN) ++ []
    in 
        computeConvexHull [] xsSorted ++ [x0]

        where 
            computeConvexHull :: [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)]
            computeConvexHull ps (x:y:z:xs) =
                let d = getDir x y z
                in case d of 
                    TurnLeft  -> computeConvexHull (x:ps) (y:z:xs)
                    _ -> case ps of
                                 (p':ps') -> computeConvexHull ps' (p':x:z:xs)
                                 _        -> computeConvexHull ps (x:z:xs)
            computeConvexHull ps x = reverse ps ++ x

            angleOrd :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Ordering
            angleOrd p@(px, py) x@(a,b) y@(c,d)
                | angle > 0 = LT
                | angle < 0 = GT
                | otherwise = if dist p x < dist p y
                              then LT
                              else GT
                where (v1x,v1y) = (a-px,b-py)
                      (v2x,v2y) = (c-px,d-py)
                      angle = atan2 (v1x*v2y - v1y*v2x) (v1x*v2x + v1y*v2y)
                  
            getDir :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
            getDir (a,b) (c,d) (e,f)
                | crossProd > 0   = TurnLeft
                | crossProd < 0   = TurnRight
                | otherwise = Straight
                where (x1,y1) = (c-a,d-b)
                      (x2,y2) = (e-c,f-d)
                      crossProd = x1*y2 - x2*y1

            dist :: (Double,Double) -> (Double,Double) -> Double
            dist (a,b) (c,d) = sqrt $ ((c-a)**2) + ((d-b)**2)
