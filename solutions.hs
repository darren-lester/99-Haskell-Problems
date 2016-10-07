import System.Random
import Data.List
-- Problem 1 Find the last element of a list.
-- using recursion
last' :: [a] -> a
last' [] = error "last': empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- using !! and length
last'' :: [a] -> a
last'' [] = error "last'': empty list"
last'' xs = xs !! (length xs - 1)

-- Problem 2 Find the last but one element of a list.
secondLast :: [a] -> a
secondLast [] = error "secondLast: empty list"
secondLast (x:xs)
 | length (x:xs) < 2 = error "secondLast: length of list less than 2" 
 | length (x:xs) == 2 = x
 | otherwise = secondLast xs

-- Problem 3 Find the K'th element of a list. The first element in the list is
-- number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "elementAt: empty list"
elementAt _ 0 = error "elementAt: 0 index"
elementAt xs i = xs !! (i - 1)

-- Problem 4 Find the number of elements of a list.
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Problem 5 Reverse a list.
-- with left fold
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldl (\acc -> \elt -> elt:acc) [] xs

-- with right fold
reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' xs = foldr (\elt -> \acc -> acc ++ [elt]) [] xs

-- Problem 6 Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == reverse' xs

-- Problem 7 Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8 Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = foldl (\acc -> \elt -> (if elt == last' acc
                                          then acc
                                          else acc ++ [elt])) [x] xs

-- Problem 9 Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate
-- sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = let group = takeWhile (== head xs) xs
              groupLength = length group in
              [group] ++ pack (drop groupLength xs)

-- Problem 10 Run-length encoding of a list. Use the result of problem P09
-- to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is
-- the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\x -> (length x, head x)) $ pack xs

-- Problem 11 Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
data ListElem a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [ListElem a]
encodeModified xs = map (\x -> if (fst x) == 1
                               then Single (snd x)
                               else Multiple (fst x) (snd x)) (encode xs)

-- Problem 12 Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

decodeModified :: [ListElem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs) = replicate n x ++ decodeModified xs

-- Problem 13 Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [ListElem a]
encodeDirect [] = []
encodeDirect (x:xs)
 | xCount == 1 = (Single x) : (encodeDirect xs)
 | otherwise = (Multiple xCount x) : encodeDirect (drop (xCount - 1) xs)
 where xCount = 1 + length (takeWhile (== x) xs)

-- Problem 14 Duplicate the elements of a list.
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs)

-- Problem 15 Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- Problem 16 Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs 1 = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

-- Problem 17 Split a list into two parts;
-- the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = foldl (\acc -> \elt -> if length (fst acc) < n
                                    then (fst acc ++ [elt], snd acc)
                                    else (fst acc, snd acc ++ [elt])) ([], []) xs

-- Problem 18 Extract a slice from a list.
-- Given two indices, i and k, the slice is the list
-- containing the elements between the i'th and k'th
-- element of the original list (both limits included).
-- Start counting the elements with 1
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k
 | i <= 0 = error "slice: start index less than 1"
 | otherwise = take (k - i + 1) $ drop (i - 1) xs

-- Problem 19 Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
 | n == 0 = xs
 | n > 0 = (drop shift xs) ++ (take shift xs)
 | n < 0 = (drop (length xs - shift) xs) ++ (take (length xs - shift) xs)
 where shift = (abs n) `mod` length xs

-- Problem 20 Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "removeAt: empty list"
removeAt i xs
 | i < 1 = error "removeAt: index less than 1"
 | i > length xs = error "removeAt: index greater than list length"
 | otherwise = (xs !! (i - 1), (take (i - 1) xs) ++ drop i xs)

-- Problem 21 Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x xs i
 | i < 1 = error "insertAt: index less than 1"
 | otherwise = take (i - 1) xs ++ [x] ++ drop (i - 1) xs

-- Problem 22 Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i j
 | i > j = error "range: start value is greater than end value"
 | i == j = [i]
 | otherwise = i : range (i + 1) j

-- Problem 23 Extract a given number of randomly selected elements from a list.
randomSelect :: Eq a => [a] -> Int -> IO [a]
randomSelect [] _ = error "randomSelect: empty list"
randomSelect xs n = do
                    g <- newStdGen
                    return $ take n (nub [ xs !! i | i <- randomRs (0, length xs - 1) g])

-- Problem 24 Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect 0 _ = do return []
diffSelect n 1 = do return [1]
diffSelect n limit
 | limit < 1 = error "diffSelect: limit less than 1" 
 | otherwise = 
   do
   g <- newStdGen
   return $ take n (nub [ x | x <- randomRs (1, limit) g])

-- Problem 25 Generate a random permutation of the elements of a list.
rndPermu :: Eq a => [a] -> IO [a]
rndPermu [] = do return []
rndPermu xs = randomSelect xs (length xs)

-- Problem 26 Generate the combinations of K distinct objects chosen from the
-- N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n - 1) xs)) ++ combinations n xs

-- Problem 28 Sorting a list of lists according to length of sublists
lengthsort :: Ord a => [[a]] -> [[a]]
lengthsort [] = []
lengthsort xs = sortBy (\x -> \y -> (length x) `compare` (length y)) xs

-- Problem 31 Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime 0 = False
isPrime x = not $ True `elem` [ x `mod` y == 0 | y <- [2..(x - 1)]]

-- Problem 32 Determine the greatest common divisor of two positive integer
-- numbers. Use Euclid's algorithm.
gcd' :: Int -> Int -> Int
gcd' x y
 | x == y = x
 | x > y = gcd' (x - y)  y
 | x < y = gcd' y x

gcd'' :: Int -> Int -> Int
gcd'' x y
 | y == 0 = x
 | otherwise = gcd'' y (x `mod` y)

-- Problem 33 Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime x y = gcd'' x y == 1