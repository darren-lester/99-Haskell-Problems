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

-- Problem 3 Find the K'th element of a list. The first element in the list is number 1.
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
compress (x:xs) = foldl (\acc -> \elt -> (if elt == last' acc then acc else acc ++ [elt])) [x] xs

-- Problem 9 Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = let group = takeWhile (== head xs) xs
              groupLength = length group in
              [group] ++ pack (drop groupLength xs)
