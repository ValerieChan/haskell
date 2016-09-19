
COMP 304

Assignment 1 

Valerie Chan 300304174

Question 1:

COUNT

>count :: (Eq a) => a -> [a] ->Int
>count item list = if list == [] then 0 --Base case (empty list)
>                  else if (head list) == item then ((count item (tail list))+1) --if the first item in the list matches the one we are looking for add one and keep counting
>                  else  (count item (tail list))-- otherwise go to the next element

I tested this by running: 

< > count 1 [1,2,1,2,1]
< > 3
< > count 1 []
< > 0

So I was happy that this function worked as it should, as it returned the same results as displayed in the assignment handout.


ALLPOS

>--allPos :: (Eq a) => a -> [a] -> [Int]
>allPos item list = if list ==[] then list --if the list is empty 
>                   else reverse(allPos' item list [] 1)-- if its not use the helper method
>allPos' item [] result index = result --when you have searched the whole list
>allPos' item list result index = if head list == item then allPos' item (tail list) (index : result) (index+1)-- if the element matches then add its index to the result.
>                                 else allPos' item (tail list) result (index +1) -- else try the next value.

Testing:
I tested this by running:

< > allPos 1 [1,2,1,2,1] 
< > [1,3,5]
< > allPos 3 [1,2,1]
< > []

So I was happy that this function worked as it should, as it returned the same results as displayed in the assignment handout.


FIRSTLASTPOS

>--firstLastPos :: (Eq a) => a -> [a] ->(a,a)
>firstLastPos item list = if list ==[] then (0,0) -- empty list case
>                         else if (allPos item list) == [] then (0,0) -- run all pos and take the first and last element.
>                         else (head (allPos item list) , last (allPos item list))


Testing:
I tested this by running:

< > firstLastPos 1 [1]
< > (1,1)
<
< > firstLastPos 1 [1,2,1,2,1] 
< > (1,5)
<
< > firstLastPos 2 [1,2,1,2,1]
< > (2,4)
<
< > firstLastPos 5 [1,2,5,2,1] 
< > (3,3)
<
< > firstLastPos 3 [1,2,1] 
< > (0,0)

So I was happy that this function worked as it should, as it returned the same results as displayed in the assignment handout.

Question 2:
I chose to implement insertion sort because insertion sort because bubble sort is not that common and insertion/selection are both pretty intuitive so I just picked one at random.  I picked merge sort for my second sort because it was mentioned in the tutorial that it was more challenging and I wanted to get more out of the assignment. I later implemented quick sort aswell just for my own interest to compare two different n Log n programs.

INSERTION SORT 

>sort1 :: (Ord a)=> [a] ->[a]
>sort1 [] = [] -- emptry list case
>sort1 [item] = [item]  -- single item list base case
>sort1 (item:xs) = insert item ( sort1 xs) -- use the helper method to insert each element as you get to it.

>insert :: (Ord a) => a -> [a] -> [a]
>insert item [] = [item] -- empty list case
>insert item (x:xs) = if item < x then item:x:xs -- go through the list checking where the element needs to be inserted.
>                     else x : insert item xs

Testing:
I first tested sort1 on the empty list case to which it returned an empty list.

< > sort1 []
< > []

Then on a one element list:

< > sort1 [1]
< > [1]

Then on a list of elements:

< > sort1 [2,3,1,5,6,2,2,1,4]
< > [1,1,2,2,2,3,4,5,6]

Because it could handle these cases I consider it functional.



MERGE SORT

>sort2 :: (Ord a) => [a] ->[a]
>sort2 [] = [] -- empty list case
>sort2 [x] = [x] -- single element case
>sort2 xs = merge(sort2 as)(sort2 bs) where (as,bs) = split xs --merge the two lists created , recursively.

>split :: [a] -> ([a], [a])
>split [] = ([],[]) -- empty case
>split [x] = ([x], []) -- one element case
>split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split(xys)

>merge :: Ord a => [a] -> [a] -> [a]
>merge xs [] = xs -- one list is empty
>merge [] ys = ys -- the other list is empty
>merge (x:xs) (y:ys) = if x < y then x:(merge  xs (y:ys)) -- take the smaller and add it to the list created by merging the rest of the other two.
>                      else y:(merge ys (x:xs))


Testing:
I repeated the same tests on sort2 to check that my implementation of merge sort worked correctly.
I tested sort2 on the empty list case to which it returned an empty list.

< > sort2 []
< > []

Then on a one element list:

< > sort2 [1]
< > [1]

Then on a list of elements:

< > sort2 [2,3,1,5,6,2,2,1,4]
< > [1,1,2,2,2,3,4,5,6]

Because it could handle these cases I consider it functional.

QUICKSORT

>sort3 :: (Ord a) => [a] -> [a]  
>sort3 [] = []  --empty case
>sort3 (x:xs) = sort3 [a | a <- xs, a <= x] ++ [x] ++ sort3 [a | a <- xs, a > x]-- sort both halfs of the list together, concatenating.
>               --sort3 [a | a <- xs, a <= x] :( [x] : (sort3 [a | a <- xs, a > x]))-- not sure why but this implementation didnt work but the other one did.

< > sort3 []
< > []

Then on a one element list:

< > sort3 [1]
< > [1]

Then on a list of elements:

< > sort3 [2,3,1,5,6,2,2,1,4]
< > [1,1,2,2,2,3,4,5,6]

Question 3: Map

>type Map a b = [(a, b)] 
>--Returns an empty map
>emptyMap :: Map a b 
>emptyMap = []

>--Check if a given key is defined in a map 
>hasKey :: (Eq a) => a->Map a a -> Bool
>--hasKey :: Int->Map Int Int -> Bool
>hasKey item [] = False
>hasKey x ((key,value) : m) = if x==key then True 
>                       else  hasKey x m --recursively check for the key

>--Set the value for a key in the map
>setVal :: (Eq a) => a -> a-> Map a a -> Map a a
>--setVal :: Int -> Int-> Map Int Int -> Map Int Int
>setVal key value [] = [(key, value)]
>setVal key value m = if (hasKey key m) == True then ((key, value) : (delKey key m))--check if the key exists already, delete it if it is there.
>                     else ((key, value) : m) -- otherwise just add it.


>--Get the value of a given key in a map
>getVal :: (Eq a ) => a -> Map a a -> a
>--getVal :: Int -> Map Int Int -> Int
>getVal x [] = error "Value not found[get]"
>getVal x ((key, value): _)  | x ==key =value --item found!
>getVal x (_:m) = getVal x m -- recursively search the rest of the map


>--Delete a key (and its value) from a map
>delKey :: (Eq a) => a -> Map a a -> Map a a 
>--delKey :: Int -> Map Int Int -> Map Int Int
>delKey key m = if (hasKey key m) == False then error "Value not found[del]" 
>               else delKey' key m
>delKey' key [] =[] --empty case for helper method
>delKey' key ((index, value): m) |key == index = delKey' key m -- we have a match..
>                                | otherwise = ((index, value) : delKey' key m) -- recursively search the rest

Testing:

For emptyMap I tried out its call and it returned the expected result.

< > emptyMap
< > []

For hasKey I tried out several calls, all giving the expected results:

< > hasKey 1 []
< > False

< > hasKey 1 [(1,2),(3,4)]
< > True

< > hasKey 1 [(0,2),(3,4)]
< > False

For setVal i used several calls to test it:

< > setVal 0 1[]
< > [(0,1)]

< > setVal 0 1[(1, 2), (3,4)]
< > [(0,1),(1,2),(3,4)]

< > setVal 0 1[(0,0),(1, 2), (3,4)]
< > [(0,1),(1,2),(3,4)]

It correctly handles the addition of an already present key.

For getVal:

< > getVal 0 []
< > *** Exception: Value not found[get]

< > getVal 0 [(0,1)]
< > 1

< > 0 [(1,2), (3,4)]
< > *** Exception: Value not found[get]

For delKey :

< > delKey 0 []
< > *** Exception: Value not found[del]

< > delKey 0 [(0,1),(2,3)]
< > [(2,3)]

< > delKey 0 [(1,1),(2,3)]
< > *** Exception: Value not found[del]


Question 4:
Build map

>buildMap :: [Int]->Map Int Int
>buildMap [] = []--empty case
>buildMap (item:xs) = buildMap' xs [(item, 1)]--use the helper then

>buildMap' [] m = m -- base case
>buildMap' (item:xs) m =if hasKey item m then buildMap' xs (setVal item ((getVal item m)+1)m ) --check if we have seen the key before, then update the value if we have
>                       else buildMap' xs ((item, 1):m)--otherwise just add it in.

Test with no double ups:

< > buildMap [1,2,3,4,5,6]
< > [(6,1),(5,1),(4,1),(3,1),(2,1),(1,1)]

Test with empty list:

< > buildMap []
< > []

Test with duplicates:

< > buildMap [1,1,1,3,1,4,2,4,3,1,1,4]
< > [(4,3),(1,6),(3,2),(2,1)]

Looking back perhaps another way to do this would have been to use the count method combined with the delKey. However this works aswell, there are many ways to do this.