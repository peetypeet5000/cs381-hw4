-- Homework 4 - Stacklang2
-- Name: Peter LaMontagne
-- Course: CS381
-- Date: 2/8/22

module Stacklang1 where

-- Grammar for the stacklang
--   S ∷=   C | C, S
--   C ∷= LD Int | ADD | MULT | DUP 

-- Data Type Definitions
type Prog = [Cmd] 

-- The supported commands
data Cmd = LD Int 
  | ADD 
  | MULT 
  | DUP 
  deriving Show

type Stack = [Int] 


-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
run :: Prog -> Stack -> Maybe Stack
run ((LD a):ps) s = run ps (a:s) -- Append a value to the stack
run ((ADD):ps) (x:y:xs) = run ps ((x + y):xs) -- Pull out two values, append their sum
run ((MULT):ps) (x:y:xs) = run ps ((x * y):xs) -- Pull out two values, append their product
run ((DUP):ps) (x:xs) = run ps (x:x:xs) -- Pull out one value, append it twice
run [] s = Just s -- Once the command list is empty, just return the stack
run _ _ = Nothing -- If no pattern match, invalid command, return Nothing
