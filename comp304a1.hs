--remember to type ":" before any comands on terminal
--just have to say you tested it? actual tests not needed. I ran count 1[23949328] and it returned this...

count :: (Eq a) => a -> [a] ->Int
count item list = if list == [] then 0 
                  else if (head list) == item then ((count item (tail list))+1) 
                  else  (count item (tail list)) --(head list) /= item then
                  
-- allPos --
--allPos :: (Eq a) => a -> [a] -> [Int]
allPos item list = if list ==[] then list
                   else reverse(allPos' item list [] 1)
allPos' item [] result index = result
allPos' item list result index = if head list == item then allPos' item (tail list) (index : result) (index+1)
                                 else allPos' item (tail list) result (index +1)
                             
--firstLastPos--
--firstLastPos :: (Eq a) => a -> [a] ->(a,a)
firstLastPos item list = if list ==[] then (0,0)
                         else if (allPos item list) == [] then (0,0)
                         else (head (allPos item list) , last (allPos item list))

--insertion sort--
--change to ords
--insert :: Int -> [Int] -> [Int]
insert :: (Ord a) => a -> [a] -> [a]
insert item [] = [item]
insert item (x:xs) = if item < x then item:x:xs
                     else x : insert item xs

--sort1 :: [Int] -> [Int]   
sort1 :: (Ord a)=> [a] ->[a]
sort1 [] = []
sort1 [item] = [item]
sort1 (item:xs) = insert item ( sort1 xs)

-- merge sort --
sort2 :: (Ord a) => [a] ->[a]
sort2 [] = []
sort2 [x] = [x]
sort2 xs = merge(sort2 as)(sort2 bs) where (as,bs) = split xs

split :: [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split(xys)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x:(merge  xs (y:ys))
                      else y:(merge ys (x:xs))
--Map q3--
--type Map a b = [(a, b)] 
--Return an empty map
--emptyMap :: Map a b 
--emptyMap = []

--Check if a given key is defined in a map
--hasKey :: Int -> Map Int Int -> Bool 
--hasKey :: (Eq a) => a->Map a a -> Bool
hasKey item [] = False
hasKey x ((key,value) : m) = if x==key then True 
                             else  hasKey x m 

--Set the value for a key in the map
--setVal :: Int -> Int -> Map Int Int -> Map Int Int
--setVal :: (Eq a) => a -> a-> Map a a -> Map a a
setVal key value [] = [(key, value)]
setVal key value m = if (hasKey key m) == True then ((key, value) : (delKey key m))
                     else ((key, value) : m)


--Get the value of a given key in a map
--getVal :: Int -> Map Int Int -> Int 
--getVal :: (Eq a ) => a -> Map a a -> a
getVal x [] = error "Value not found[get]"
getVal x ((key, value): _)  | x ==key =value
getVal x (_:m) = getVal x m


--Delete a key (and its value) from a map
--delKey :: Int -> Map Int Int -> Map Int Int 
--delKey :: (Eq a) => a -> Map a a -> Map a a 
delKey key m = if (hasKey key m) == False then error "Value not found[del]" 
               else delKey' key m
delKey' key [] =[]
delKey' key ((index, value): m) |key == index = delKey' key m
                                | otherwise = ((index, value) : delKey' key m)
--Build map-- 
--buildMap :: [Int]->Map Int Int
--buildMap [] = []
--buildMap (item:xs) = buildMap' xs [(item, 1)]

--buildMap' [] m = m
--buildMap' (item:xs) m =if hasKey item m then buildMap' xs (setVal item ((getVal item m)+1)m ) 
--                       else buildMap' xs ((item, 1):m)
--perhaps should have used the count method combined with the delete key. however this works too so it doesnt seem to matter.