import System.IO
import Control.Monad


readInt :: String -> Int
readInt = read

main = do
        contents <- readFile "day1nums.txt"
        print . map readInt . words $ contents


getFile :: IO [Int]
getFile = do
        contents <- readFile "day1nums.txt"
        return (map readInt . words $ contents)


performOperation1 = do
                    fileContent <- getFile
                    let leftSide = everyEven fileContent
                    let rightSide = everyOdd fileContent
                    let sortLeft = mergeSort leftSide
                    let sortRight = mergeSort rightSide
                    print (sum (map abs (sumDifferences sortLeft sortRight)))


performOperation2 = do
                    fileContent <- getFile
                    let leftSide = everyEven fileContent
                    let rightSide = everyOdd fileContent
                    print (sum (mult2Lists (map (`inlist` rightSide) leftSide) leftSide))


inlist :: Int -> [Int] -> Int
inlist _ [] = 0
inlist target (x:xs) | x == target = 1 + inlist target xs
                     | otherwise = inlist target xs

sumDifferences:: [Int] -> [Int] -> [Int]
sumDifferences [] [] = []
sumDifferences (x:xs) (y:ys) = (x - y) : sumDifferences xs ys

mult2Lists :: [Int] -> [Int] -> [Int]
mult2Lists [] [] = []
mult2Lists (x:xs) (y:ys) = (x * y) : mult2Lists xs ys


--format data to get right lists
everyEven ::[Int] ->[Int]
everyEven [] = []
everyEven (x:y:xs) = x : everyEven xs

everyOdd :: [Int] -> [Int]
everyOdd [] = []
everyOdd (x:y:xs) = y : everyOdd xs


--mergesort code from ages ago
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y:ys) = y : merge [] ys
merge (x:xs) [] = x : merge xs []
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge ys zs
              where
                ys = mergeSort (fst (halve xs))
                zs = mergeSort (snd (halve xs))