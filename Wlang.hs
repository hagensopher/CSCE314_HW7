
-- Assignment 7, CSCE-314
-- Hagen Sopher
--UIN: 426004814
module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- AST definition for W
data WValue = VInt Int 
            | VBool Bool 
              deriving (Eq, Show) --can i add this?

data WExp = Val WValue

          | Var String

          | Plus WExp WExp
          | Minus WExp WExp
          | Multiplies WExp WExp
          | Divides WExp WExp

          | Equals WExp WExp
          | NotEqual WExp WExp
          | Less WExp WExp
          | Greater WExp WExp
          | LessOrEqual WExp WExp
          | GreaterOrEqual WExp WExp

          | And WExp WExp
          | Or WExp WExp
          | Not WExp
--exec :: WStmt -> Memory -> Memory takes a statement (if,delcare,while) and the block of memory and returns the block of memeory
data WStmt = Empty
           | VarDecl String WExp
           | Assign String WExp
           | If WExp WStmt WStmt
           | While WExp WStmt
           | Block [WStmt]

type Memory = [(String, WValue)]
marker = ("|", undefined)
isMarker (x, _) = x == "|"

-- eval function
eval :: WExp -> Memory -> WValue
--eval = undefined
--eval for int and bool
eval (Val (VInt x)) myMemory = VInt x
eval (Val (VBool y)) myMemory = VBool y
--Val case (dont get what string is)
eval (Var myString) myMemory = fromJust(lookup myString myMemory)

--Plus (like hw6)
eval (Plus (Val (VInt x) ) (Val (VInt y))) myMemory = VInt(x+y)
eval (Plus x y) mm = eval (Plus (Val (eval x mm)) (Val (eval y mm))) mm

--Minus (Like HW 6)
eval (Minus (Val (VInt x) ) (Val (VInt y))) myMemory = VInt(x-y)
eval (Minus x y) mm = eval (Minus (Val (eval x mm)) (Val (eval y mm))) mm

--Multiply (Like HW6)
eval (Multiplies (Val (VInt x) ) (Val (VInt y))) myMemory = VInt(x*y)
eval (Multiplies x y) mm = eval (Multiplies (Val (eval x mm)) (Val (eval y mm))) mm

--Divide (new)
eval (Divides (Val (VInt x) ) (Val (VInt y))) myMemory = VInt(div x y)
eval (Divides x y) mm = eval (Divides (Val (eval x mm)) (Val (eval y mm))) mm

--Equals (Like HW6)
--eval (Equals (Val (VBool x)) (Val (VBool y))) mm = VBool(x == y) --maybe not needed
eval (Equals x y) mm = VBool ((eval x mm) == (eval y mm))

--Not Equal (New)
--eval (NotEqual (Val (VBool x)) (Val (VBool y))) mm = VBool(x /= y) -- maybe not needed
eval (NotEqual x y) mm = VBool ((eval x mm) /= (eval y mm))

--Less (New)
--eval (Less (Val (VBool x)) (Val (VBool y))) mm = VBool(x < y) -- maybe not needed
eval (Less x y) mm = VBool (asInt(eval x mm) < asInt(eval y mm))

--LessOrEqual (New)
--eval (LessOrEqual (Val (VBool x)) (Val (VBool y))) mm = VBool(x <= y) -- maybe not needed
eval (LessOrEqual x y) mm = VBool (asInt(eval x mm) <= asInt(eval y mm))

--Greater (New)
--eval (Greater (Val (VBool x)) (Val (VBool y))) mm = VBool(x > y) -- maybe not needed
eval (Greater x y) mm = VBool (asInt(eval x mm) > asInt(eval y mm))

--GreaterOrEqual (New)
--eval (GreaterOrEqual (Val (VBool x)) (Val (VBool y))) mm = VBool(x <= y) -- maybe not needed
eval (GreaterOrEqual x y) mm = VBool (asInt(eval x mm) >= asInt(eval y mm))

--AND (New)
eval (And (Val (VBool True)) (Val (VBool True))) mm = VBool True 
eval (And (Val (VBool _)) (Val (VBool _))) mm = VBool False 
eval (And x y) mm = eval (And (Val (eval x mm)) (Val (eval y mm))) mm

--Or (New)
--eval (Or (Val (VBool x)) (Val (VBool y))) mm = VBool(x || y) -- maybe not needed
eval (Or (Val (VBool False)) (Val (VBool False))) mm = VBool False 
eval (Or (Val (VBool _)) (Val (VBool _))) mm = VBool True 
eval (Or x y) mm = eval (Or (Val (eval x mm)) (Val (eval y mm))) mm
--Not (New)
eval (Not (Val (VBool True))) mm = VBool False
eval (Not (Val (VBool False))) mm = VBool True
eval (Not x) mm =  (eval x mm)


-- exec function
exec :: WStmt -> Memory -> Memory

exec (VarDecl name thing) mm =  if (lookup name mm) == Nothing then (name, eval thing mm) : mm   else  error "Variable already declared"  --is this right???? how does memory work???
exec (Assign name thing) mm = if (lookup name mm) /= Nothing then help2 mm name (eval thing mm) else error "Variable was not declared"  --- i know this is wrong lol
exec (Block stmtlist) mm = (helperBlock stmtlist mm2)  --need recusion for the block!!!
  where mm2 = ("|",VInt (-1)) : mm
exec (If exp stmt1 stmt2) mm = if asBool(eval exp mm) then exec stmt1 mm else exec stmt2 mm  --both stmt1 and stmt2 can be recusive like if (5<3) > (6<2)
exec (While exp stmt1) mm = if asBool(eval exp mm) then exec (While exp stmt1) (exec stmt1 mm) else mm 

-- helpher function for block
helperBlock :: [WStmt] -> Memory -> Memory
helperBlock [] mm = dropMemory mm -- CALL DROP MEMORY HERE maybe when a block is done clear the memory with a new function
helperBlock (x:xs) mm = (helperBlock xs (exec x mm))
--removed the memory that is no longer used
dropMemory:: Memory -> Memory
dropMemory (x:xs) = if not (isMarker x) then dropMemory xs else xs

--helps with assining
help:: Memory -> String -> Int -> Int 
help (x:xs) f count = if fst x == f then count else help xs f (count+1)
--second function to help with assigning
help2:: Memory -> String -> WValue -> Memory
help2 myList name newV = take count myList ++ [(name,newV)] ++ drop (count+1) myList
  where count = (help myList name 0 )


-- example programs
result = lookup "result" ( exec factorial [("result", undefined), ("x", VInt 10)] )

--BOTH TEST CASESE WORK 

factorial = 
  Block
  [
    VarDecl "acc" (Val (VInt 1)),
    While (Greater (Var "x") (Val (VInt 1)))
    (
      Block
      [
        Assign "acc" (Multiplies (Var "acc") (Var "x")),
        Assign "x" (Minus (Var "x") (Val (VInt 1)))         
      ]
    ),
    Assign "result" (Var "acc")
  ]

p1 = Block
     [
       VarDecl "x" (Val (VInt 0)),
       VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
       If (Or (Var "b") (Not (GreaterOrEqual (Var "x") (Val (VInt 0)))))
         ( Block [ Assign "x" (Val (VInt 1)) ] )
         ( Block [ Assign "x" (Val (VInt 2)) ] )
     ]

-- some useful helper functions
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

-- unit tests
myTestList =

  TestList [
    test $ assertEqual "p1 test" [] (exec p1 []),

    let res = lookup "result" (exec factorial [("result", undefined), ("x", VInt 10)])
    in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res))
    ]    

-- main: run the unit tests  
main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          if (errs + fails /= 0) then exitFailure else return ()

--HERE IS THE FIBINOCCI SEQUENCE 

fibonacci :: Int -> Int
fibonacci n = asInt(fromJust(lookup "result" (exec fibH [("result",undefined),("n",VInt n)])))

fibH = Block
  [
    VarDecl "x" (Val (VInt 0)),
    VarDecl "y" (Val (VInt 1)),
    VarDecl "count" (Val (VInt (1))),
    If (Equals (Var "n") (Val(VInt 0))) --if x == 0 then x = 0
      (Block [Assign "result"(Val(VInt 0))])
      (Block [ --elif x ==1 then x =0
        If (Equals (Var "n") (Val(VInt 1)))
          (Block [Assign "result"(Val(VInt 1))])
          (Block [
              
              While (Less (Var "count")(Var "n"))
                (Block
                  [
                    Assign "result" (Plus (Var "x") (Var "y")),
                    Assign "x" (Var "y"),
                    Assign "y" (Var "result"),
                    Assign "count" (Plus (Var "count") (Val (VInt 1)))
                  ])
              
                   
          ])
      ])
  ]

--OTHER METHOD TO DO IT RECUSIVILY

-- fibonacci2:: Int -> Int
-- fibonacci2 n = asInt (fromJust q)
--   where q = lookup "result" (exec (fhelp [("result",undefined),("n",VInt n)] n) [])


-- fhelp :: Memory -> Int-> WStmt
-- fhelp mm n = Block
--   [
--     If (Equals (Var "x") (Val(VInt 0))) --if x == 0 then x = 0
--       (Block [Assign "result"(Val(VInt 0))])
--       (Block [ --elif x ==1 then x =0
--         If (Equals (Var "x") (Val(VInt 1)))
--           (Block [Assign "result"(Val(VInt 1))])
--           (Block [
--               Assign "result" (Val(eval (Plus (Val(VInt(fibonacci2 (n-1)))) (Val(VInt(fibonacci2 (n-2)))))[]))
                   
--           ])
--       ])
--   ]
--   where n = asInt(fromJust(lookup "n" mm))