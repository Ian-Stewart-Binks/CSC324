-- Reimplement all of the list functions from the first set of exercises in
-- the Racket chapter, this time in Haskell, with or without using explicit
-- recursion.

-- 1. Write a function to determine the length of the list
len :: [a] -> Int
len = foldl (\x y -> x + 1) 0

-- 2. Write a function to determine if a given item appears in a list.
elem2 :: Eq a => a -> [a] -> Bool
elem2 item = foldl (\x y -> y == item || x) False

-- 3. Write a function to determine the number of duplicates in a list.
dup :: Eq a => [a] -> Int
dup [] = 0
dup lst = (dup $ tail lst) + (if elem (head lst) (tail lst) then 1 else 0)

-- 4. Write a function to remove all duplicates from a list.
removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup lst = (if elem (head lst) (tail lst) then [] else [(head lst)]) ++ (removeDup $ tail lst)

-- 5. Given two lists, output the items that appear in both lists (intersection).
--    Then, output the items that appear in at least one of the two
--    lists (union).
intersection :: Eq a => [a] -> [a] -> [a]
intersection lst1 lst2 = filter (`elem` lst2) lst1

union_ :: Eq a => [a] -> [a] -> [a]
union_ lst1 lst2 = removeDup $ lst1 ++ lst2

-- 6. Write a function which takes a list of lists, and returns the list which
-- contains the largest item (e.g., given ’((1 2 3) (45 10) () (15)),
-- return ’(45 10)).
largestSubItem :: Ord a => [[a]] -> [a]
largestSubItem lst = foldl (\x y -> if null x then y else if null y then x else if maximum x > maximum y then x else y) [] lst