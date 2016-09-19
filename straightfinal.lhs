Assignment 3
Comp 304
Valerie Chan 300304174


I have completed parts 1,2,3 to the best of my interpretation. I have attempted 4 but it is not in the most fantastic state so it has been commented out (so will not run but is still visible).

Compiler and interpreter for simple straight line programs.

A straight line program is just a list of assignment statements, where an
expression can contain variables, integer constants and arithmetic operators.

> data Prog = Prog [Stmt]
>             deriving (Eq, Show)

> data Stmt = Asgn Var Exp 
>             | DeclareInt Var
>             | DeclareBool Var 
>             | Skip 
>             | If Cond Prog Prog 
>             | While Cond Prog 
>             | Procedure Prog 
>             |End 
>             -- | Procedure2 [(Var, Val)] Prog 
>             deriving (Eq, Show)

> data Cond = Cond RelOp Exp Exp | CondB BoolOp Exp Exp | CondBN BoolOp Exp
>             deriving (Eq, Show)

 data Method = Prog 
             deriving (Eq, Show)

> data Exp = Const Val
>          | Var Char
>      	   | Bin Op Exp Exp
>          | Rel RelOp Exp Exp
>          | Boolean BoolOp Exp Exp
>          | BooleanNot BoolOp Exp 
>            deriving (Eq, Show)

> data Op = Plus | Minus | Times | Div
>           deriving (Eq, Show)

> data RelOp = Eq | Ne | Lt | Le | Gt | Ge | Check
>             deriving (Eq, Show)

> data BoolOp = And | Or | Not
>               deriving (Eq, Show)

The store is a list of variable names and their values.  For now, a variable
now is just a single character and a value is an integer.

> type Store = [(Var, Val)]
 
> type Var = Char

> data Val = Num Int | Flag Bool
>            deriving (Eq, Show)

Straight line programs are translated into code for a simple stack-oriented
virtual machine.

A VM code program is a list of commands, where a command is either a load
immdiate (which loads a constant onto the stack), load (which loads the value
of a variable onto the stack), sstore (which saves the value at the top of the
stack into memory, and deletes it), or an arithmetic operation (which applies
an operations to the two values at the top of the stack and replaces them by
the result).

> type Code = [Command]

> data Command = LoadI Val 
>               | Load Var 
>               | DecInt Var 
>               | DecBool Var
>               | Store Var 
>               | BinOp Op 
>               | RelOpOp RelOp 
>               | BoolOpOp BoolOp 
>               | BoolOpNot BoolOp
>               | LJump Int 
>               | Noop 
>               | Label Int 
>               | LJumpEQ Int 
>               | MethodCall Int 
>               | MethodLabel Int 
>               | LJumpBack Int Val 
>               | JumpStack
>                deriving (Show, Eq)


I have added many commands, these are : RelOpOp, BoolOpOp, BoolOpNot, LJump, Noop, (Parts 1, 2) 
Label, LJumpEQ, MethodCall, MethodLabel, LJimpBack, JumpStack (Parts 3).

> type Stack = [ Val ]

Run a program, by compiling and then executing the resulting code
The program is run with an initially empty store and empty stack, and the
output is just the resulting store.  This could be modified to provide an
initial store.

> out :: Prog -> IO ()
> out prog = print (thirdPass (secondPass 0 code code))
>           where code = (translate prog) ++ [Label 1000]

Out prints out the code. I added this additional testing function so that I could see 
the code produced by the compiler. 

> run :: Prog -> Store
> run prog = snd (exec (thirdPass (secondPass 0 code code))  0 ([], []) (thirdPass (secondPass 0 code code)))
>      	     where code = (translate prog) ++ [Label 1000]

I added the Label 1000 to represent the end of the program, this is neccesary so that once procedures are added 
the program does not keep going past where it is meant to. I used 1000 as the label needed to be an int and I 
wanted it to be sufficiently high that it would be unlikely to be used in regular label assignment.


Translate straight line program into stack machine code

> translate :: Prog -> Code
> translate (Prog stmts) = trans stmts [] 0 

> trans :: [Stmt] -> [Prog] ->Int -> Code
> trans [] [] _ = []
> trans [] (x:procedureList) num = ((MethodLabel num):(translate x))++((JumpStack) :( trans [] procedureList (num+1) ) )
> trans (Procedure p : stmts) procedureList num = if (stmts == []) then (MethodCall (getIndex p (addToList  p procedureList) 0) ):(trans stmts (addToList p  procedureList) (0))
>                                                  else  (MethodCall (getIndex p (addToList  p procedureList) 0) ):(trans stmts (addToList p  procedureList) (num+1))
> trans (stmt : stmts) procedureList num =if (stmts ==[]) then  (trans' stmt num ) ++ (trans stmts procedureList (0) ) 
>                                         else  (trans' stmt num ) ++ (trans stmts procedureList (num+2) )


for part 4 I want to add the assignments of the variables passed through to be assigned at the start of the procedure.

> -- trans (Procedure2 variables p : stmts) procedureList num = if (stmts == []) then (MethodCall (getIndex p' (addToList  p' procedureList) 0) ):(trans stmts (addToList p'  procedureList) (0))
>  --                                               else  (MethodCall (getIndex p' (addToList  p' procedureList) 0) ):(trans stmts (addToList p'  procedureList) (num+1))
>  --                                                where p' = addToFront p variables
 
 I imagine I will need a method that adds these on to the front of the program, i will add this in here:
 
> -- addToFront:: Prog -> [(Var,Val)]-> Prog
> -- addToFront program [] = program
> -- addToFront program ((name ,(Num x)):xs) = addToFront ((DeclareInt name):( (Asgn name (Num x)):program)) xs
> -- addToFront program ((name ,(Flag x)):xs) = addToFront ((DeclareBool name):( (Asgn name (Flag x)):program ) xs
                                                  


In part 3 I used the commands MethodLabel to signify the begining of a procedure.
This matches with an integer used in MethodCall. Method call has the index that 
we jump to and the index to jump back from. It places the return location on the 
stack so it can be accessed later.

I keep a list of procedures so that each one is only translated once.


> addToList :: Prog ->[Prog] -> [Prog]
> addToList newOne [] = [newOne]
> addToList newOne (x: list) = if x == newOne then x:list
>                              else (x:(addToList newOne list))

> getIndex :: Prog -> [Prog] -> Int -> Int
> getIndex p [] count = -1
> getIndex p (x:list) count = if x ==p then count
>                             else getIndex p list (count+1)

> size :: [a] -> Int -> Int
> size [] count = count
> size (x:xs) count = size xs (count+1)

> trans' :: Stmt -> Int ->Code
> trans' Skip num = []
> trans' (DeclareInt var) num= [DecInt var]
> trans' (DeclareBool var) num = [DecBool var]
> trans' (Asgn var exp) num = (transexp exp) ++ [Store var]
> trans' (If cond thenPart elsePart) labelCount= (transcond cond) ++ ((LJumpEQ (labelCount) ):((translate thenPart) ++ ((LJump (labelCount+1)) : ((Label (labelCount)) : ((translate elsePart) ++ [Label (labelCount+1)])))))
> trans' (While cond prog) labelCount = (Label (labelCount) :((transcond cond) ++((LJumpEQ (labelCount+1)) :(translate prog ++ [(LJump (labelCount)), (Label (labelCount+1))]))))
> trans' (End) num = [LJump 1000] 

I originally simplified the line jumps within the program to be label based so 
that when I was looking for where to jump I simply looked for the correct label. 
However after some disscussion in tutorials I extended it so that it was implemented 
to jump to an address rather than a name as this more closely resembles machine code.
I have implemented the labeling so that each new statement gets its a new unused integer 
label.


> transexp :: Exp -> Code
> transexp (Const n) = [LoadI n]
> transexp (Var v) = [Load v]
> transexp (Bin op e1 e2) = transexp e1 ++ transexp e2 ++ [BinOp op]
> transexp (Rel relop e1 e2) = transexp e1 ++ transexp e2 ++ [RelOpOp relop]
> transexp (Boolean boolop e1 e2) = transexp e1 ++ transexp e2 ++ [BoolOpOp boolop]
> transexp (BooleanNot boolop e1) = transexp e1 ++ [BoolOpNot boolop]

> transcond :: Cond -> Code
> transcond (Cond relop e1 e2) = transexp e1 ++ transexp e2 ++ [RelOpOp relop]
> transcond (CondB boolop e1 e2) = transexp e1 ++ transexp e2 ++ [BoolOpOp boolop]


> -- Secondpass updates the code so that Jumplabels become jump instruction addresses.--
> secondPass :: Int ->Code -> Code -> Code
> secondPass line original [] = []
> secondPass line original ((LJumpEQ n):program) = (LJumpEQ (returnPc  n original 0)) : (secondPass (line+1) original program)
> secondPass line original ((LJump n):program) = (LJump (returnPc n original 0)) : (secondPass (line+1) original program)
> secondPass line original ((MethodCall n ):program) = (LJumpBack (returnPcM  n original 0) (Num (line+1) ) ) :(secondPass (line+1) original program)
> secondPass line original  (x:program) = (x :(secondPass (line+1) original program))


> -- thirdPass turns all address labels into noops.--
> thirdPass :: Code -> Code
> thirdPass [] = []
> thirdPass (Label _ :program) = Noop : (thirdPass program)
> thirdPass (MethodLabel _ :program) = Noop: (thirdPass program)
> thirdPass (x:program) = x: (thirdPass program)


> -- Returns the code from the line number we are looking for--
> returnPc :: Int ->Code -> Int -> Int
> returnPc lookingfor [] pc = error "no match"
> returnPc lookingfor (Label n :program) pc = if (n == lookingfor) then pc 
>                                       else returnPc lookingfor program (pc+1)
> returnPc lookingfor (x:program) pc= returnPc lookingfor program (pc+1)

> returnPcM :: Int ->Code -> Int -> Int
> returnPcM lookingfor [] pc = error "no match"
> returnPcM lookingfor (MethodLabel n :program) pc = if (n == lookingfor) then pc 
>                                       else returnPcM lookingfor program (pc+1)
> returnPcM lookingfor (x:program) pc= returnPcM lookingfor program (pc+1)


> returnPcC :: Int ->Code -> Int -> Int
> returnPcC lookingfor [] pc = error "no match"
> returnPcC lookingfor ((MethodCall n) :program) pc = if (n == lookingfor) then pc 
>                                       else returnPcC lookingfor program (pc+1)
> returnPcC lookingfor (x:program) pc= returnPcC lookingfor program (pc+1)



Execute a stack code program

> exec :: Code -> Int ->(Stack, Store) -> Code -> (Stack, Store)
> exec [] pc ss original = ss
> exec (LJump n : cmds) pc ss original = exec (ljump n original 0) (n+1) ss original
> exec (LJumpBack go home : cmds) pc (stack, store) original = exec (ljump (go) original 0 ) (go) (home:stack, store) original
> exec (JumpStack : cmds) pc ((Num x):stack, store) original = exec (ljump x original 0) (x) (stack, store) original
> exec (JumpStack : cmds) pc ((Flag x):stack, store) original = error "top of stack is boolean"
> exec (LJumpEQ n : cmds) pc (x:stack, store)  original= if x == (Flag False) then exec (ljump n original 0) (n) (stack, store) original 
>                                            else exec cmds (pc+1)(stack, store) original
> exec (cmd : cmds) pc ss original = exec cmds (pc+1) (exec' cmd ss) original                        

I filter out the jump instructions in exec rather than exec' becuase I then have better 
access to the program count (pc), the original code and rest of the commands. 
I have added some checks to make sure that a boolean value is not accidently used as a jump location.

> exec' :: Command -> (Stack, Store) -> (Stack, Store)
> exec' (LoadI n) (stack, store) = (n:stack, store)
> exec' (Load v) (stack, store) = (x:stack, store)
> 	                             where x = getVal v store
> exec' (DecInt v) (stack, store) = if (contains v store ) then error "variable already declared"-- if store does not contain v
>                                   else  (stack, store')
> 	                             where store' = setVal v (Num 0) store
> exec' (DecBool v) (stack, store) = if (contains v store ) then error "variable already declared"-- if store does not contain v
>                                   else  (stack, store')
> 	                             where store' = setVal v (Flag False) store
> exec' (Store v) (x:stack, store) = if (contains v store) then if((applyc Check x (getVal v store)) == (Flag True)) then (stack, store') else error "declaration does not match variable"
>                                    else error "use of undeclared variable"
> 	                             where store' = setVal v x store
> exec' (BinOp op)  (x:y:stack, store) = (z:stack, store)
> 	                             where z = apply op y x
> exec' (RelOpOp op) (x:y:stack, store) = (z:stack, store)
> 	                             where z = applyc op y x
> exec' (BoolOpOp op) (x:y:stack, store) = (z:stack, store)
>                                where z = applyb op y x
> exec' (BoolOpNot op) (x:stack, store) = (z:stack, store)
>                                 where z = applyn op x 
> exec' (LJump n) ss = error "ljump missing"
> exec' (Label n) ss = error "labels no longer part of syntax"
> exec' (LJumpEQ n) ss = error "ljumpEq missing"
> exec' (LJumpBack n n2) ss = error "ljumpback missing"
> exec' (JumpStack) ss = error "jumpstack missing"
> exec' (MethodLabel n) ss = error "ML"
> exec' (MethodCall n) ss = error "MC"
> exec' (Noop) ss = ss
> --  exec' _ ss = ss


I implemented ljump to return the code from that line onwards. This is good 
for handling while loops and jumpbacks aswell as it starts again at the begining
of the original code.

> ljump :: Int -> Code -> Int -> Code
> ljump label [] pc = error "no such jump location"
> ljump label (x: cmds) pc= if (label == pc) then x:cmds
>                           else ljump label cmds (pc+1)


Apply an arithmetic operator
instance Num x where
    fromInteger :: Integer -> x
    
> apply :: Op -> Val -> Val -> Val
> apply Plus (Num x) (Num y) = (Num (x+y) )
> apply Minus (Num x) (Num y) = Num (x-y)
> apply Times (Num x) (Num y) = Num (x*y)
> apply Div (Num x) (Num y) = Num (x `div` y)

Apply a relational operator

> applyc :: RelOp -> Val -> Val -> Val
> applyc Eq (Flag x) (Flag y) = Flag (x == y)
> applyc Ne (Flag x) (Flag y) = Flag (x /= y)
> applyc Lt (Flag x) (Flag y) =  Flag (x < y)
> applyc Le (Flag x) (Flag y) = Flag (x <= y)
> applyc Gt (Flag x) (Flag y) = Flag (x > y)
> applyc Ge (Flag x) (Flag y) = Flag (x >= y)
> applyc Eq (Num x) (Num y) = Flag (x == y)
> applyc Ne (Num x) (Num y) = Flag (x /= y)
> applyc Lt (Num x) (Num y) = Flag (x < y)
> applyc Le (Num x) (Num y) = Flag (x <= y)
> applyc Gt (Num x) (Num y) = Flag (x > y)
> applyc Ge (Num x) (Num y) = Flag (x >= y)
> applyc Check (Num x)(Num y) = Flag True
> applyc Check (Flag x)(Flag y) = Flag True
> applyc _ _ _ = error "wrong type applied"

> applyb :: BoolOp -> Val -> Val -> Val
> applyb And (Flag True) (Flag True) = (Flag True) 
> applyb And (Flag  _) (Flag _) = (Flag False)
> applyb Or (Flag _) (Flag True) = (Flag True)
> applyb Or (Flag True) (Flag _) = (Flag True)
> applyb Or (Flag False) (Flag False) = (Flag False)
> applyb _ (Num _) (Num _) = error "wrong type applied"

I have assumed And, Or and Not to use the regular truth tables such as studied in math161.

> applyn :: BoolOp -> Val -> Val 
> applyn Not (Flag False) = (Flag True)
> applyn Not (Flag True) = (Flag False)
> applyn _ _  = error "wrong type applied"

This may not be the most elegant way to to implement this but it 
is functioning. I couldnt work out how to do overriding in haskell 
so that it my methods would take multiple different types of 
Operation. This is why there are several versions of apply.

I ended up using the applyc to type check my variables, the check
makes sure that two variables of different types are not used with each other.
I am able to use this internally.


Contains

> contains :: Var -> Store -> Bool
> contains v [] = False
> contains v ((var, val):s) = if (v == var) then True
>                            else contains v s

Look up a variable in the store

> getVal :: Var -> Store -> Val
> getVal v s = foldr (\(u,x) r -> if u==v then x else r) (error "Variable not found") s

Assign a value to a variable in the store

> setVal :: Var -> Val -> Store -> Store
> setVal v x [] = [(v,x)]
> setVal v x ((u,y):s) | v == u = (v,x):s
> 	     	           | otherwise = (u,y):setVal v x s



Some examples for testing Part 1


if (0 < 1) then x = 1 else x=0

> zero = Const (Num 0)
> one = Const (Num 1)
> decx = DeclareInt 'x'

> c = Cond Lt zero one 
> thenPart = Asgn 'x' one
> elsePart = Asgn 'x' zero

> pthenPart = Prog [thenPart]
> pelsePart = Prog [elsePart]

> ifTest = Prog [decx, If c pthenPart pelsePart, End]

output:
*Main> run ifTest
[('x',Num 1)]
*Main> out ifTest
[DecInt 'x',LoadI (Num 0),LoadI (Num 1),RelOpOp Lt,LJumpEQ 8,LoadI (Num 1),
Store 'x',LJump 11,Noop,LoadI (Num 0),Store 'x',Noop,LJump 13,Noop]

> two = Const (Num 2)
> decy = DeclareInt 'y'
> asx = Asgn 'x' zero
> asy = Asgn 'y' two
> varx = Var 'x'
> vary = Var 'y'
> w = Cond Lt varx vary
> d = Bin Plus varx one
> c1 = Prog [Asgn 'x' d]
> whileTest = Prog[decx, decy, asx ,asy, While w c1 , End]

x = 0
y = 2
while (x < y)
    x = x+1
output:
*Main> run whileTest
[('x',Num 2),('y',Num 2)]
*Main> out whileTest
[DecInt 'x',DecInt 'y',LoadI (Num 0),Store 'x',LoadI (Num 2),
Store 'y',Noop,Load 'x',Load 'y',RelOpOp Lt,LJumpEQ 16,Load 'x',
LoadI (Num 1),BinOp Plus,Store 'x',LJump 6,Noop,LJump 18,Noop]



Part 2:

testing use of boolean :

> g = Rel Eq (Const (Flag True)) (Const (Flag False))
> boolTest = Prog [DeclareBool 'x', Asgn 'x' g]

*Main> run boolTest
[('x',Flag False)]



Testing use of And:

> h =  Boolean And (Const (Flag False)) (Const (Flag True))
> andTest = Prog [DeclareBool 'x', Asgn 'x' h]

*Main> run andTest
[('x',Flag False)]

> h2 =  Boolean And (Const (Flag True)) (Const (Flag True))
> andTest2 = Prog [DeclareBool 'x', Asgn 'x' h2]

*Main> run andTest2
[('x',Flag True)]



Testing use of Not:

> h3 = BooleanNot Not h2
> notTest = Prog [DeclareBool 'x', Asgn 'x' h3]

*Main> run notTest
[('x',Flag False)]


boolean op as conditions:

> con = CondB And (Const (Flag True)) (Const (Flag True))
> thenPart2 = Asgn 'x' (Const (Num 1))
> elsePart2 = Asgn 'x' (Const (Num 0))
> pthenPart2 = Prog[thenPart2]
> pelsePart2 = Prog[elsePart2]
> boolCondTest = Prog [DeclareInt 'x', If con pthenPart2 pelsePart2]

*Main> run boolCondTest
[('x',Num 1)]


testing undeclared variable:

> undec = Prog [asx,End]

*Main> run undec
*** Exception: use of undeclared variable


testing double declarations:

> doubledec = Prog [decx ,decy]

*Main> run doubledec
*** Exception: variable already declared


testing incorrect types given :

> i = Rel Eq (Const (Flag True)) (Const (Num 8))
> boolTestType = Prog [DeclareBool 'x', Asgn 'x' i]

*Main> run boolTestType
[('x',*** Exception: wrong type applied


> boolDecTestType = Prog [DeclareInt 'x', Asgn 'x' g]

*Main> run boolDecTestType
*** Exception: wrong type applied



Part 3:
I wrote the procedure function so that any regular program could effectively be used as a procedure, the only issue could be if variables end up being declared twice. Therefore it is important that functions be carefully written. I implemented it so that on a procedure call the procedure would be added to a list (if not already there), and then translated later. There were several issues implementing this because it was hard to know where the end of the function would be. Second and Third pass were useful functions to enable labels to be changed to jumps and noops added. I recursively translate procedure calls.

Test procedure max:

> --Max :assign z to the max of  l,r
> e1 = Prog [Asgn 'z' (Var 'r')]
> e2 = Prog [Asgn 'z' (Var 'l')]
> c3 = Cond Lt (Var 'l')(Var 'r')
> maxT = Prog [ If c3 e1 e2]
> maxT2 = Prog [ If c3 e2 e1]

> r = Asgn 'r' (Const (Num 6))
> l = Asgn 'l' (Const (Num 7))
> r2 = Asgn 'r' (Const (Num 10))
> l2 = Asgn 'l' (Const (Num 20))
> testProcedure = Prog [DeclareInt 'r', DeclareInt 'l', DeclareInt 'z', r2,l2,Procedure maxT, r, l, Procedure maxT, End]

*Main> run testProcedure
[('r',Num 6),('l',Num 7),('z',Num 7)]
*Main> out testProcedure
[DecInt 'r',DecInt 'l',DecInt 'z',LoadI (Num 10),Store 'r',LoadI (Num 20),Store 'l',LJumpBack 14 (Num 8),LoadI (Num 6),Store 'r',LoadI (Num 7),Store 'l',LJumpBack 14 (Num 13),LJump 27,Noop,Load 'l',Load 'r',RelOpOp Lt,LJumpEQ 22,Load 'r',Store 'z',LJump 25,Noop,Load 'l',Store 'z',Noop,JumpStack,Noop]


Now testing my procedure function on programs with multiple different procedures:

> testMultipleProcedures = Prog [DeclareInt 'r', DeclareInt 'l', DeclareInt 'z', r2,l2,Procedure maxT2, r, l, Procedure maxT, End]

*Main> out testMultipleProcedures
[DecInt 'r',DecInt 'l',DecInt 'z',LoadI (Num 10),Store 'r',LoadI (Num 20),Store 'l',LJumpBack 14 (Num 8),LoadI (Num 6),Store 'r',LoadI (Num 7),Store 'l',LJumpBack 27 (Num 13),LJump 40,Noop,Load 'l',Load 'r',RelOpOp Lt,LJumpEQ 22,Load 'l',Store 'z',LJump 25,Noop,Load 'r',Store 'z',Noop,JumpStack,Noop,Load 'l',Load 'r',RelOpOp Lt,LJumpEQ 22,Load 'r',Store 'z',LJump 25,Noop,Load 'l',Store 'z',Noop,JumpStack,Noop]

*Main> run testMultipleProcedures
[('r',Num 6),('l',Num 7),('z',Num 6)]


Part 4:

I began implementing the additional feature of being able to pass variables I added an additional Procedure2 statement type. I wasnt sure about how to pass through the variable names. I tried several ideas :
-Pass through just integers and assign them inside,

> -- part4Test = Prog [ ,Procedure2 [('w', (Num 3)), ('v', (Num 4))] min), ]

-Pass through just the name of the variable and in my translation load that variable onto the stack and then save it into prenamed variables eg r and l.

The second one is better because it is closer to the normal use of variable arguments in other programing Languages. However I ran out of time to fully implement this. Below is a rough idea of what a test for it would look like

 > -- e11 = Prog [Asgn 'z' (Var 'r')]
 > -- e22 = Prog [Asgn 'z' (Var 'l')]
 > -- c4 = Cond Lt (Var 'l')(Var 'r')
 > -- min = Prog[If c4 e11 e22]
 > -- part4Test = [asx ,asy, DeclareInt 'z', Procedure2 ['x', 'y'] min]
