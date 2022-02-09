module Stacklang1 where

-- Grammar for the stacklang
--   S ∷=   C | C, S
--   C ∷= LD Int | ADD | MULT | DUP 

-- Data Type Definitions

type Prog = [Cmd] 

type Stack = [Either Bool Int]

data Cmd  = LDI Int 
  | LDB Bool 
  | LEQ 
  | ADD 
  | MULT 
  | DUP 
  | IFELSE Prog Prog 
  deriving Show

-- Program Constructors

-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
run :: Prog -> Stack -> Maybe Stack
run ((LDI a):ps) s = run ps (a:s)
run ((LDB a):ps) s = run ps (a:s)
run ((ADD):ps) (x:y:xs) = run ps ((x + y):xs)
run ((MULT):ps) (x:y:xs) = run ps ((x * y):xs)
run ((DUP):ps) (x:xs) = run ps (x:x:xs)
run ((LEQ):ps) (x:y:xs)
  | (x <= y) = run ps ((Left True):xs)
  | otherwise = run ps ((Left False):xs)
run ((IFELSE prog1 prog2):ps) (x:xs)
  | (x == True) = run prog1 xs
  | otherwise = run prog2 xs
run [] s = Just s
run _ _ = Nothing


-- Sample Test Programs

stack1 :: Stack
stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
stack2 :: Stack
stack2 = [Left True, Right 3] 
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
