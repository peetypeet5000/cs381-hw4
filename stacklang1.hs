module Stacklang1 where

-- Grammar for the stacklang
--   S ∷=   C | C, S
--   C ∷= LD Int | ADD | MULT | DUP 

-- Data Type Definitions

type Prog = [Cmd] 

data Cmd = LD Int 
  | ADD 
  | MULT 
  | DUP 
  deriving Show

type Stack = [Int] 

-- Program Constructors

-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
run :: Prog -> Stack -> Maybe Stack
run ((LD a):ps) s = run ps (a:s)
run ((ADD):ps) (x:y:xs) = run ps ((x + y):xs)
run ((MULT):ps) (x:y:xs) = run ps ((x * y):xs)
run ((DUP):ps) (x:xs) = run ps (x:x:xs)
run [] s = Just s
run _ _ = Nothing


-- Sample Test Programs

stack1::Stack
stack1 = [1, 2, 3, 4, 5] 
test1 = [LD 3,DUP,ADD,DUP,MULT]  
test2 = [LD 3,ADD] 
test3 = [] 
test4 = [ADD, ADD, ADD, ADD]
