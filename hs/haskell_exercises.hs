-- Page 55.
-- 1. Reimplement all of the list functions from the first set of exercises in
--    the Racket chapter, this time in Haskell, with or without using explicit
--    recursion.

-- 1.1. Write a function to determine the length of the list
len :: [a] -> Int
len = foldl (\x y -> x + 1) 0

-- 1.2. Write a function to determine if a given item appears in a list.
elem2 :: Eq a => a -> [a] -> Bool
elem2 item = foldl (\x y -> y == item || x) False

-- 1.3. Write a function to determine the number of duplicates in a list.
dup :: Eq a => [a] -> Int
dup [] = 0
dup lst = (dup $ tail lst) + (if elem (head lst) (tail lst) then 1 else 0)

-- 1.4. Write a function to remove all duplicates from a list.
removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup lst = (if elem (head lst) (tail lst) then [] else [(head lst)]) ++ (removeDup $ tail lst)

-- 1.5. Given two lists, output the items that appear in both lists (intersection).
--    Then, output the items that appear in at least one of the two
--    lists (union).
intersection :: Eq a => [a] -> [a] -> [a]
intersection lst1 lst2 = filter (`elem` lst2) lst1

union_ :: Eq a => [a] -> [a] -> [a]
union_ lst1 lst2 = removeDup $ lst1 ++ lst2

-- 1.6. Write a function which takes a list of lists, and returns the list which
-- contains the largest item (e.g., given ’((1 2 3) (45 10) () (15)),
-- return ’(45 10)).
largestSubItem :: Ord a => [[a]] -> [a]
largestSubItem lst = foldl (\x y -> if null x then y else if null y then x else if maximum x > maximum y then x else y) [] lst

type Stack = [Integer]
type StackOp a = Stack -> (a, Stack)

pop :: StackOp Integer
pop (top:rest) = (top, rest)

push :: Integer -> StackOp ()
push item stack = ((), item : stack)

-- 2. Make sure you understand the meaning of seq, and how to use it correctly.
-- Seq takes in two arguments. It sort of evaluates the first one, and then
-- returns the second one.

-- 3. Define an infinite list containing all negative numbers. Then, de-
--    fine an infinite list ints containing all integers such that elem x ints
--    halts whenever x is an integer.
negNums :: [Integer]
negNums = -1 : (map (\x -> x - 1) negNums)

-- 4.

-- Page 82 Exercises
-- 1. Implement the functions removeSecond :: StackOp () and
--    removeThird :: StackOp (), which remove the second-highest and
--    third-highest item from the stack, respectively. (Here “highest” refers
--    to an item’s position on the stack, and not the item’s value.)
removeSecond :: StackOp ()
removeSecond [] = ((), [])
removeSecond s = let (first, s1) = pop s
                     (second, s2) = pop s1
                 in push first s2


removeThird :: StackOp ()
removeThird [] = ((), [])
removeThird s = let (first, s1) = pop s
                    (_, s2) = removeSecond s1
                 in push first s2

-- 2. Implement the function removeNth :: Integer -> StackOp (), which
--    takes a positive integer n and returns a StackOp that removes the nthhighest
--    from the stack.
removeNth :: Integer -> StackOp ()
removeNth _ [] = ((), [])
removeNth 1 (x:xs) = ((), xs)
removeNth n (x:xs) = let (_, s1) = removeNth (n - 1) xs
                     in ((), x:s1)

-- 3. One of the drawbacks of pop is that it raises a runtime error on So in case you’ve been lulled into a
--    false sense of security by static typing,
--    runtime errors occur in Haskell too!
--    an empty stack. Implement safePop :: StackOp (Maybe Integer),
--    which behaves similarly to pop, except it does not have this problem
safePop :: StackOp (Maybe Integer)
safePop [] = (Nothing, [])
safePop (x:xs) = (Just x, xs)

-- 4. Implement returnVal :: a -> StackOp a, which takes a value and
--    returns a new StackOp with no mutating effect, and which simply
--    returns the value as the “result.”
-- > returnVal 5 [4,2,6,10]
-- (5, [4,2,6,10])
-- > returnVal [True,False] [4,2,6,10]
-- ([True,False], [4,2,6,10])
returnVal :: a -> StackOp a
returnVal n s = (n, s)

-- 5. Using returnVal, sumOfStack so that it does not “mutate” the stack.


(>~>) :: StackOp a -> (a -> StackOp b) -> StackOp b
(f >~> g) s = let (x, s1) = f s
                  newStackOp = g x
              in newStackOp s1

(>>>) :: StackOp a -> StackOp b -> StackOp b
(f >>> g) s = (f >~> \x -> g) s

(~>) :: StackOp a -> (a -> b) -> StackOp b
(f ~> g) s = (f >~> \x y -> (g x, y)) s



sumOfStack :: StackOp Integer
sumOfStack [] = (returnVal 0 [])
sumOfStack s = (pop >~> \x -> sumOfStack ~> (+x) >~> \y -> push x >>> returnVal y) s

-- 6. Using returnVal, re-implement removeSecond, removeThird, and removeNth
--    so that they also return the removed item.
removeSecondPrime :: StackOp Integer
removeSecondPrime = pop >~>
                    \y -> pop >~>
                    \x -> push y >>>
                    returnVal x

removeThirdPrime :: StackOp Integer
removeThirdPrime = pop >~>
                   \x -> pop >~>
                   \y -> pop >~>
                   \z -> push y >>
                   push x >>>
                   returnVal z


removeNthP :: Integer -> StackOp Integer
removeNthP _ [] = (0, [])
removeNthP 1 (x:xs) = (x, xs)
removeNthP n s = (pop >~> \x ->
                  removeNthP (n - 1) >~>
                  \y -> push x >>
                  returnVal y) s

-- 7. Implement len :: StackOp Integer, which computes the number
--    of elements on the stack. Do not “mutate” the stack.
leng :: StackOp Integer
leng [] = (0, [])
leng s  = (pop >~> \x ->
           leng ~> (+1) >~> \y -> push x >> returnVal y) s


-- 8. Implement stackMap :: (Integer -> Integer) -> StackOp (),
--    which takes a function f and mutates the stack by applying f to every
--    item.

stackMap :: (Integer -> Integer) -> StackOp ()
stackMap _ [] = ((), [])
stackMap f s = (pop >~> \x -> (stackMap f) >> push (f x)) s

-- 9. Implement stackFilter :: (Integer -> Bool) -> StackOp (),
--    which creates a StackOp that removes all elements in the stack that
--    don’t satisfy the input predicate.
stackFilter :: (Integer -> Bool) -> StackOp ()
stackFilter _ [] = ((), [])
stackFilter f s = (pop >~> \x -> stackFilter f >>>
                   if f x then push x else returnVal ()) s

stackFoldl :: (a -> Integer -> a) -> a -> StackOp a
stackFoldl f a [] = (a, [])
stackFoldl f ac s = (pop >~> \x -> (stackFoldl f (f ac x)) >~> \y -> push x >>> returnVal y) s

