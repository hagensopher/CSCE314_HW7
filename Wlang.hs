{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Assignment 7, CSCE-314

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
eval (And (Val (VBool True)) (Val (VBool True))) mm = VBool True -- maybe not needed
eval (And (Val (VBool _)) (Val (VBool _))) mm = VBool False -- maybe not needed
eval (And x y) mm = eval (And (Val (eval x mm)) (Val (eval y mm))) mm

--Or (New)
--eval (Or (Val (VBool x)) (Val (VBool y))) mm = VBool(x || y) -- maybe not needed
eval (Or (Val (VBool False)) (Val (VBool False))) mm = VBool False -- maybe not needed
eval (Or (Val (VBool _)) (Val (VBool _))) mm = VBool True -- maybe not needed
eval (Or x y) mm = eval (Or (Val (eval x mm)) (Val (eval y mm))) mm
--Not (New)
eval (Not (Val (VBool True))) mm = VBool False-- maybe not needed
eval (Not (Val (VBool False))) mm = VBool True-- maybe not needed
eval (Not x) mm =  (eval x mm)



--type Memory = [(String, WValue)] memory again for access
--values for testing
f = Val (VInt 4)
g = VarDecl "acc" f
-- exec function
exec :: WStmt -> Memory -> Memory
--exec = undefined
--exec (VarDecl name thing) mm = []
exec (VarDecl name thing) mm =  if (lookup name mm) == Nothing then (name, eval thing mm) : mm   else  error "Variable already declared"  --is this right???? how does memory work???
exec (Assign name thing) mm = if (lookup name mm) /= Nothing then help2 mm name (eval thing mm) else error "Variable was not declared"  --- i know this is wrong lol
exec (Block stmtlist) mm = (helperBlock stmtlist mm2)  --need recusion for the block!!!
  where mm2 = ("|",VInt (-1)) : mm
exec (If exp stmt1 stmt2) mm = if asBool(eval exp mm) then exec stmt2 mm else exec stmt1 mm  --both stmt1 and stmt2 can be recusive like if (5<3) > (6<2)
exec (While exp stmt1) mm = if asBool(eval exp mm) then exec (While exp stmt1) (exec stmt1 mm) else mm 

-- helpher function for block
helperBlock :: [WStmt] -> Memory -> Memory
helperBlock [] mm = mm -- CALL DROP MEMORY HERE maybe when a block is done clear the memory with a new function
helperBlock (x:xs) mm = (helperBlock xs (exec x mm))
--maybe need a while blcok helper similar to helperBlock
--whileHelper:: WStmt -> Memory -> Memory
--whileHelper blck mm =  blck (exec blck mm)


dropMemory:: Memory -> Memory
dropMemory (x:xs) = if not (isMarker x) then dropMemory xs else xs


help:: Memory -> String -> Int -> Int 
help (x:xs) f count = if fst x == f then count else help xs f (count+1)

help2:: Memory -> String -> WValue -> Memory
help2 myList name newV = take count myList ++ [(name,newV)] ++ drop (count+1) myList
  where count = (help myList name 0 )
-- example programs
result = lookup "result" ( exec factorial [("result", undefined), ("x", VInt 10)] )
-- easy test case 
factorial2 = 
  Block
  [
    VarDecl "i" (Val (VInt 0)),
    VarDecl "acc" (Val (VInt 0)),
    VarDecl "result" (Val (VInt 0)),
    While (Less (Var "i") (Val (VInt 3)))
    (
      Block
      [
        Assign "acc" (Plus (Var "acc") (Var "i")),
        Assign "i" (Plus (Var "i") (Val (VInt 1)))         
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

ahh:: [Int] -> Int -> Int-> Int
ahh (x:xs) f count = if x == f then count else ahh xs f (count + 1)

ahh2:: [Int] -> Int -> Int-> [Int]
ahh2 myList oldV newV = take count myList ++ [newV] ++ drop (count+1) myList
  where count = (ahh myList oldV 0 )

