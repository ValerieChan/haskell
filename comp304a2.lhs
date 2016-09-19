Valerie Chan 300304174

Question 1:

> data BinTree a = Empty | Node a (BinTree a) (BinTree a)
>                  deriving Show

> --Test if tree contains node x
> hasbt ::(Eq a) => a -> BinTree a -> Bool
> hasbt x (Empty) = False --emtpy base case
> hasbt x (Node a l r) = if (x==a) then True 
>                        else if (hasbt x l ) then True --search each side of the tree
>                        else if (hasbt x r ) then True
>                        else False

Testing:
*Main> let tree1 = (Node 2 (Empty) (Node 1 (Node 3 (Empty)(Empty))(Empty)))

Testing a valid input:
*Main> hasbt 3 tree1
True

Testing an empty tree: 
*Main> hasbt 3 Empty
False

Testing a list not in the tree:
*Main> hasbt 8 tree1
False

>--Test whether trees t1 and t2 are identical; i.e. are both empty, or have
>--the same label at the root and the same subtrees.
> equalbt ::(Eq a)=> BinTree a -> BinTree a -> Bool 
> equalbt Empty (Node a2 l2 r2) = False -- the trees are different heights so not equal 
> equalbt (Node a1 l1 r1) Empty= False -- again here
> equalbt Empty Empty = True -- a leaf is the same height
> equalbt (Node a1 l1 r1) (Node a2 l2 r2) = if (a1 /= a2) then False --nodes have different values
>                                           else if (((equalbt l1 l2)== True) && ((equalbt r1 r2)== True)) then True
>                                           else False --run it on both sides if they are both true then true

Tested here:

*Main> let tree = (Node 2 (Empty) (Node 1 (Empty)(Empty)))
*Main> let tree1 = (Node 2 (Empty) (Node 1 (Node 3 (Empty)(Empty))(Empty)))

Testing two trees the same:
*Main> equalbt tree tree
True

Testing different trees:
*Main> equalbt tree tree1
False


>--Construct a mirror image of tree t
> reflectbt :: BinTree a -> BinTree a
> reflectbt Empty = Empty -- reached a leaf
> reflectbt (Node a1 l r ) = Node a1 (reflectbt r) (reflectbt l)  --switch the children around 


Testing:
Testing an empty tree:
*Main> reflectbt Empty
Empty

Testing a simple tree:
*Main> let tree = (Node 2 (Empty) (Node 1 (Empty)(Empty)))
*Main> reflectbt tree
Node 2 (Node 1 Empty Empty) Empty

Testing a more complex tree:
*Main> let tree = (Node 2 (Empty) (Node 1 (Empty)(Node 3 Empty (Node 4 Empty Empty))))
*Main> reflectbt tree
Node 2 (Node 1 (Node 3 (Node 4 Empty Empty) Empty) Empty) Empty

> --Construct the fringe of tree t; i.e. a list containing the labels on the leaves of
>--the tree, in the order they would be visited in a left-to-right depth-first traversal.
> fringebt :: BinTree a -> [a]
> fringebt Empty = []
> fringebt (Node x Empty Empty) = [x] -- return the value this is the base case
> fringebt (Node x l Empty) = fringebt l 
> fringebt (Node x Empty r) = fringebt r
> fringebt (Node x l r) = (fringebt l ) ++ (fringebt r) --add each side together


Testing:
Testing on empty:
*Main> fringebt Empty
[]

Testing on tree:
let tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty)))(Node 6 Empty Empty)
*Main> fringebt tree
[3,5,6]

>-- Check whether tree t is full; i.e. if every node has either 0 or 2 subtrees.
> fullbt :: BinTree a -> Bool
> fullbt Empty = True -- base case
> fullbt (Node x Empty Empty) = True -- 0 subtrees
> fullbt (Node x Empty r) = False -- 1 subtree :(
> fullbt (Node x l Empty ) = False --1 subtree :(
> fullbt (Node x l r) = if( fullbt l ) then (fullbt r) -- if one side is full then check the other otherwise 
>                       else False-- dont bother


Testing:

*Main> let tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty)))(Node 6 Empty Empty)

Test on a unbalanced tree:
*Main> fullbt tree
False
*Main> let tree = Node 1 (Node 2 Empty Empty)(Node 3 Empty Empty)

Test on a balanced tree:
*Main> fullbt tree
True

Test on an empty tree:
*Main> fullbt Empty
True


Question 2:

> btfold :: (a ->b ->b ->b )-> b -> BinTree a -> b
> btfold function unit Empty = unit
> btfold function unit (Node x l r) = function x (btfold function unit l) (btfold function unit r)

btfold works by taking a function and recursively folding both sides of the tree. I originally struggled with this as I did not realise btfold would take 4 arguments, i had interpreted it as 3. This way is much easier!

> hasbtf :: (Eq a) => a -> BinTree a -> Bool
> hasbtf item Empty = False 
> hasbtf item (Node x l r) = btfold (\u v w -> if (item==u) then True 
>                                               else if v then True 
>                                               else if w then True 
>                                               else False) False (Node x l r ) 

Testing (for btfold indirectly...):
Testing the empty case:
*Main> hasbtf 4 Empty
False

Testing a valid case:
*Main> hasbtf 2 (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))
True

Testing a bad input:
*Main> hasbtf potato (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))
<interactive>:230:8: Not in scope: ‘potato’

Testing an out of tree value:
*Main> hasbtf 7 (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))
False



equalbtf isnt possible because fold takes one binary tree and we need to give it two. This could probably be implemented very complexly with some sort of messy recursive function but I do not think an elegant simple solution within the scope of this course exists.

> reflectbtf :: BinTree a -> BinTree a
> reflectbtf Empty = Empty
> reflectbtf (Node x l r ) = btfold (\u v w -> (Node u w v) ) Empty (Node x l r)

In reflectbt the function given swaps the order of the nodes.

Testing:
A full case:
*Main> reflectbtf (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))

An Empty case:
Node 1 (Node 3 (Node 4 Empty Empty) Empty) (Node 2 Empty Empty)
*Main> reflectbtf Empty
Empty


> fringebtf :: (Eq a) => BinTree a -> [a]
> fringebtf Empty = []
> fringebtf (Node x l r) = btfold (\u v w -> if v == [] && w == [] 
>                                                then [u]
>                                             else v++w) [] (Node x l r)

If im at a leaf node add the label onto the list.
I used ++ concatenation because i want sure how to do it a more efficient way within the function.

Testing:
*Main> fringebtf (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))
[2,4]
*Main> fringebtf Empty
[]

> fullbtf :: BinTree a -> Bool
> fullbtf Empty = True
> fullbtf (Node x l r) = btfold (\u v w -> if v && w then True 
>                                          else if v==False && w==False then True
>                                          else False) False (Node x l r)

An empty tree I considered to be full because for all nodes that exist (0), each has 2 or 0 subtrees (still 0).

Testing:
*Main> fullbtf (Node 1 (Node 2 Empty Empty)(Node 3 Empty (Node 4 Empty Empty)))
False
*Main> fullbtf Empty
True

Question 3:

setting up a bst for testing:
*Main> let bst = Node 4 (Node 2 (Node 1 Empty Empty)(Node 3 Empty Empty))(Node 6 (Node 5 Empty Empty)(Node 7 Empty Empty))

> --Return an empty BST.
> empty :: BinTree a
> empty = Empty

> --Insert an item into a BST (no change if already there).
> insert :: (Ord a)=> a -> BinTree a -> BinTree a
> insert item Empty = (Node item Empty Empty) -- base case
> insert item (Node x l r) = if x== item then (Node x l r) -- it already exists...
>                            else if item > x then (Node x l (insert item r)) -- go left to find its spot
>                            else (Node x (insert item l) r ) -- go right
> 

Another way to implement this would have been to 'flatten' the tree, insert the item into the appropriate part of the list and create a new bst. This however would have an order cost of greater than log n as it would have the log n flatten and then each add would be log n, aswell as the cost of inserting into a list.

The empty case:
*Main> insert 4 Empty
Node 4 Empty Empty

Inserting an existing value:
*Main> insert 3 bst
Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) (Node 7 Empty Empty))

Inserting a new value:
*Main> insert 10 bst
Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) (Node 7 Empty (Node 10 Empty Empty)))

I tried testing an insert of -1. This did not work because Haskell treats negatives differently.

> --Check whether a given item occurs in a BST.
> has ::(Ord a) => a -> BinTree a -> Bool
> has a Empty = False
> has a (Node x l r) = if a == x then True
>                      else if (a > x) then has a r
>                      else has a l

 A trivial solution for this would be to use either the hasbt or the hasbtf from earlier in the assignment, however this would not make use of the nature of the BST and we would end up searching the whole tree instead of the small amount we do here.


> --Delete a given item from a BST (no change if not there).
> delete :: (Ord a) =>a -> BinTree a -> BinTree a
> delete item Empty = error "Deletion from an empty tree"
> delete item (Node x Empty Empty) = Empty -- base case
> delete item (Node x Empty r) = r
> delete item (Node x l Empty) = l
> delete item (Node x l r) = if item == x then let (p, q)=(del' l) in
>                                                 (Node p q r)
>                            else if item > x then Node x l (delete item r)
>                            else Node x (delete item l) r

I need a helper function to pull the next element up into place.

> --del' pulls up the rightmost element of the left subtree, and 
> -- deletes that element from its orignial place.
> del' ::(Ord a ) => BinTree a -> (a , BinTree a )
> del' (Node x Empty Empty) = (x, Empty)
> del' (Node x l Empty) = (x,  l)
> del' (Node x Empty r) = del'(r)

Testing:

Delete on an element that is present:
*Main> delete 7 bst
Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) Empty)

Delete on an element that isnt present:
*Main> delete 10 bst
Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) Empty)

Delete on an empty list:
*Main> delete 2 Empty
*** Exception: Deletion from an empty tree

I could have flattened it removed the item from the list (which is easy) and then rebuild the tree in order. However it would no longer be NlogN.


> --Returns a list of the items in a BST. (in order)
> flatten :: BinTree a -> [a]
> flatten Empty = []
> flatten (Node x Empty Empty) = [x]
> flatten (Node x l r) = (flatten l) ++( x : (flatten r))

Here i do a depth first traversal so all the nodes are in order.


Testing:
*Main> flatten bst
[1,2,3,4,5,6,7]
*Main> flatten Empty
[]
*Main> flatten (Node "hi" Empty Empty)
["hi"]

> --Determines whether two BSTs contain
> --the same items. DOESNT HAVE TO HAVE THE SAME STRUCTURE...
> equals :: (Eq a) =>BinTree a -> BinTree a -> Bool 
> equals Empty (Node x1 l1 r1) = False
> equals (Node x1 l1 r1) Empty = False
> equals (Node x1 l1 r1) (Node x2 l2 r2) = check (flatten (Node x1 l1 r1))( flatten(Node x2 l2 r2))

I need a helper method to check the two lists are the same: 

> check ::(Eq a)=> [a]-> [a] -> Bool
> check [] [] = True
> check [] _ = False
> check _ [] = False
> check (x:xs) (y:ys) = if x ==y then check xs ys
>                       else False

delete all the elements from one into the other and if you are left with 2 empty trees then true.
This could be done trivially (but expensively) by flattening both and checking that the lists equal. 


Testing:

*Main> equals bst Empty
False
*Main> equals bst bst
True
*Main> equals Empty bst
False

Test two trees with the same items but a different stucture:
*Main> let bst1 = (Node 5 (Node 2 Empty (Node 3 Empty Empty))(Node 6 Empty Empty))
*Main> let bst2 = (Node 5 (Node 3 (Node 2 Empty Empty)Empty )(Node 6 Empty Empty))
*Main> equals bst1 bst2
True

Test two different trees:
*Main> let bst3 = (Node 4 (Node 3 Empty Empty) Empty)
*Main> equals bst2 bst3
False

Question 4:

Part a):

> type Graph a = [(a,Int,a)]

> allpaths ::(Eq a)=> a -> a -> Graph a -> Graph a
> allpaths x y g = if x == y then [(x,0,y)]
>                            else findPath (firstSuccessor x y g) y g [] []
>


>--find path calls successor recursively on the graph that it is given.
> findPath :: (Eq a)=> Graph a ->a->Graph a ->[a] -> Graph a -> Graph a --do visited here
> findPath [] goal graph visited res = res
> findPath ((a,b,c):list) goal graph visited res = (successor (a,b,c) goal graph visited res)++( findPath list goal graph (a:visited) res)


To test find path:
*Main> let g = [("a",1,"c"),("a",1,"b"),("c",1,"d"),("b",1,"e"), ("b",2,"f"),("d",1,"f"), ("d",2,"f"), ("e",1,"f") ]
*Main> findPath [("a",1,"c"),("a",1,"b")] "f" g [] []
[("a",2,"d"),("a",3,"f"),("a",2,"e")]
*Main> 


> --firstSuccessor start goal graph returns a list
> firstSuccessor start goal [] = []
> firstSuccessor start goal ((a,b,c):graph) = if start ==goal then error "start equals goal"
>                                             else if (start == a )then (a,b,c):firstSuccessor start goal graph
>                                             else firstSuccessor start goal graph


>--Takes an edge and a list of edges and makes a list of child edges combined with the original node.
> combine :: (Eq a)=>(a,Int,a) -> Graph a -> Graph a
> combine (a,b,c) [] = []
> combine (a,b,c) (x:list) = (condense (a,b,c) x ): (combine (a,b,c) list)

> condense :: (Eq a) => (a,Int,a) -> (a,Int,a) -> (a,Int,a)
> condense (a,b,c) (d,e,f) = if (c == d)then (a, (b+e), f) 
>                             else (d, (b+e), c)

>--Successor takes an edge and returns a list of child edges combined with the original node. 
>-- effectively draws a new edge between the start node and the child node of equal value to the path taken.
> successor :: (Eq a)=> (a,Int,a)-> a ->Graph a -> [a]-> Graph a-> Graph a
> successor (x,y,z) goal [] visited res= combine (x,y,z) res  
> successor (x,y,z) goal ((a,b,c):list) visited res = if (z ==a ) then successor (x,y,z) goal list (a :visited)  ((a,b,c): res)
>                                                     else successor (x,y,z) goal list (a:visited) res 

I needed some additional helper methods:

>--helper methods
> nodes ::(Eq a)=> Graph a -> [a] ->[a]
> nodes [] list = list
> nodes ((a,_,b):g) list = if (contains a list)== False && (contains b list) == False then nodes g (b:(a:list))
>                           else if (contains a list)== False then nodes g (a:list)
>                           else if (contains b list) == False then nodes g (b:list)
>                           else nodes g list

> contains ::(Eq a)=> a ->[a] -> Bool
> contains item [] = False
> contains item (x:xs) = if (item == x) then True
>                        else contains item xs

Assuming that my allpaths works then these methods will also work:

> reachable x y [] = False
> reachable x y g = reachfind x y (allpaths x y g)

> reachfind :: (Eq a) => a -> a -> Graph a -> Bool
> reachfind x y [] = False
> reachfind x y ((a,b,c):g) = if a== x && y == c then True
>                           else (reachfind x y g)

Testing reachable with present and absent values:
*Main> let g = [("a",1,"c"),("a",1,"b"),("c",1,"d"),("b",1,"e"), ("b",2,"f"),("d",1,"f"), ("d",2,"f"), ("e",1,"f") ]
*Main> reachable "a" "d" g
True
*Main> reachable "a" "r" g
False


> minCost x y [] = error "no graph"
> minCost x y g = minfind 100000000 (allpaths x y g)
>

> minfind :: Int -> Graph a -> Int
> minfind current [] = current
> minfind current ((a,b,c):g) = if b < current then minfind b g
>                           else minfind current g

Testing minfind:
*Main> minfind 10000 [("c",3,"f"),("c",2,"f")]
2

Testing minfind:
*Main> minCost "a" "d" g
2

The result of lazy evaluation means that we have to do more 'work' to find the minCost because we have to check and find what the lowest value is. Because of the way I wrote my allpaths function it actually isnt that much extra work. I condense each path into the sum of is edges with the start and end nodes. I still have to check which one is the shortest though.

(b) find the different components (ie 2 bits that arent connected..)


> cliques :: (Eq a)=> Graph a -> Graph a
> cliques [] = []
> cliques ((a,b,c):g) =(cliques' ((a,b,c):(firstSuc a  g)) ((a,b,c):g) ) ++((a,000,a): cliques (removeGraph (cliques' ((a,b,c):(firstSuc a  g)) ((a,b,c):g) ) g))

> removeGraph :: (Eq a)=>Graph a -> Graph a-> Graph a
> removeGraph [] g = g
> removeGraph ((a,b,c):bad) g = removeGraph bad (removeItem (a,b,c ) g)

> removeItem :: (Eq a)=> (a,Int,a)->Graph a -> Graph a
> removeItem _ [] = []
> removeItem (a,b,c) ((x,y,z):ys) |( a == x && b==y && c==z )   = removeItem (a,b,c) ys
>                     | otherwise = (x,y,z) : removeItem (a,b,c) ys

> contains' ::(Eq a)=> (a,Int,a) ->Graph a -> Bool
> contains' item [] = False
> contains' (x,y,z) ((a,b,c):xs) = if (a == x && y==b && z==c) then True
>                        else contains (x,y,z) xs

> cliques' :: (Eq a)=> Graph a -> Graph a -> Graph a 
> cliques' [] g  = [] 
> cliques' g []  = g 
> cliques' ((a,b,c):g) graph =(a,b,c):((firstSuc c graph) ++ (cliques' g graph)) 


Testing cliques':
*Main> cliques' [("a",1,"c")] g
[("a",1,"c"),("c",1,"d")]

*Main> cliques' [("a",1,"b")] g
[("a",1,"b"),("b",1,"e"),("b",2,"f")]

*Main> cliques' [("a",1,"b"), ("a",1,"c")] g
[("a",1,"b"),("b",1,"e"),("b",2,"f"),("a",1,"c"),("c",1,"d")]

> --firstSuccessor start goal graph returns a list
> firstSuc :: (Eq a)=> a -> Graph a -> Graph a
> firstSuc start [] = []
> firstSuc start ((a,b,c):graph) = if (start == a )then ((a,b,c):(firstSuc start graph))
>                                  else firstSuc start graph

Testing firstSuc
*Main> ("a",1,"c"): firstSuc "c" g
[("a",1,"c"),("c",1,"d")]

*Main> let g = [("a",1,"c"),("a",1,"b"),("c",1,"d"),("b",1,"e"), ("b",2,"f"),("d",1,"f"), ("d",2,"f"), ("e",1,"f") ,("i",1,"j"), ("j",1,"k")]

Testing Clique:
*Main> cliques g
[("a",1,"c"),("c",1,"d"),("a",1,"b"),("b",1,"e"),("b",2,"f"),("a",0,"a"),("d",1,"f"),("d",2,"f"),("d",0,"d"),("e",1,"f"),("e",0,"e"),("i",1,"j"),("j",1,"k"),("i",0,"i")]

I am able to find all the edges that are connectable from each of the nodes in the graph. I have placed a "seperator" which is of the form ("d",0,"d") but I was unsure make a list of seperate trees. I have implemented a method which removes things we have already seen. There is a bug in my firstSuc method which means that it only finds 2 levels down [my alternative being an infinite loop], this results in lower levels being represented as another graph. However these are the only duplicates. I understand that this will not be in a form that will pass the auto marking but I had no success in implementing a [Graph a].