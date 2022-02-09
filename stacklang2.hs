-- Homework 4 - Stacklang2
-- Name: Peter LaMontagne
-- Course: CS381
-- Date: 2/8/22

module Stacklang2 where

-- Data Type Definitions

type Prog = [Cmd] 

-- The stack supports Bools and Ints
type Stack = [Either Bool Int]

-- These are the commands supported
data Cmd  = LDI Int 
  | LDB Bool 
  | LEQ 
  | ADD 
  | MULT 
  | DUP 
  | IFELSE Prog Prog 
  deriving Show


-- Semantic definition of how to run a program
-- Run commands on a stack until no commands are left
run :: Prog -> Stack -> Maybe Stack
run ((LDI a):ps) s = run ps ((Right a):s) -- Append a on the stack
run ((LDB a):ps) s = run ps ((Left a):s) -- Same as above, but a Left bool type
run ((ADD):ps) ((Right x):(Right y):xs) = run ps ((Right (x + y)):xs) -- Add with INTs only
run ((MULT):ps) ((Right x):(Right y):xs) = run ps ((Right (x * y)):xs) -- Mult with INTs only
run ((DUP):ps) (x:xs) = run ps (x:x:xs) -- Dupe with either type, just con twice
run ((LEQ):ps) (x:y:xs) -- Append a Left True/False depending on condition
  | x <= y = run ps ((Left True):xs)
  | otherwise = run ps ((Left False):xs)
run ((IFELSE prog1 prog2):ps) (Left x:xs) -- Append the program instructions, depending on condition
  | x == True = run (prog1 ++ ps) xs
  | otherwise = run (prog2 ++ ps) xs
run [] s = Just s -- Once out of commands, return the stack
run _ _ = Nothing -- If it does not pattern match, it is an invalid command
