{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Assignment 7, CSCE-314

module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- AST definition for W
data WValue = VInt Int 
            | VBool Bool 
              deriving (Eq, Show,Ord) --can i add this?

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
--eval (Var myString) myMemory = myString

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
eval (Less x y) mm = VBool ((eval x mm) < (eval y mm))

--LessOrEqual (New)
--eval (LessOrEqual (Val (VBool x)) (Val (VBool y))) mm = VBool(x <= y) -- maybe not needed
eval (LessOrEqual x y) mm = VBool ((eval x mm) <= (eval y mm))

--Greater (New)
--eval (Greater (Val (VBool x)) (Val (VBool y))) mm = VBool(x > y) -- maybe not needed
eval (Greater x y) mm = VBool ((eval x mm) > (eval y mm))

--GreaterOrEqual (New)
--eval (GreaterOrEqual (Val (VBool x)) (Val (VBool y))) mm = VBool(x <= y) -- maybe not needed
eval (GreaterOrEqual x y) mm = VBool ((eval x mm) >= (eval y mm))

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
exec (VarDecl name thing) mm = mm ++ [(name, eval thing mm)] --is this right???? how does memory work???
exec (Assign name thing) mm = mm ++ [(name, eval thing mm)] --- i know this is wrong lol
exec (Block stmtlist) mm = ("|",undefined) : mm --need recusion for the block!!!
exec (If exp stmt1 stmt2) mm = eval exp   --both stmt1 and stmt2 can be recusive like if (5<3) > (6<2)







-- example programs
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