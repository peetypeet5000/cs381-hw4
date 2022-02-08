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

data Maybe Stack = Just Stack | Nothing

-- Program Constructors

run ∷ Prog → Stack → Maybe Stack

semCmd ∷ Cmd →Stack→ Maybe Stack 


semCmd (LD n ) s =  Just (n:s)

-- Sample Test Programs

stack1 = [1, 2, 3, 4, 5] 
test1 = [LD 3,DUP,ADD,DUP,MULT]  
test2 = [LD 3,ADD] 
test3 = [] 
test4 = [ADD, ADD, ADD, ADD]








buildNAnd :: Noun -> Noun -> Noun
buildNAnd n1 n2 = NAnd n1 n2

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | Recursivly builds until only one element remains
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Silly Cats) Hug Dogs)
conjunction :: [Sentence] -> Sentence
conjunction (x:xs) 
  | (xs == []) = x
  | otherwise = And x (conjunction(xs))

-- | Pretty print a sentence.
-- | Matches all possible sentence structures to print them
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs = "dogs"
prettyNoun Ducks = "ducks"
prettyNoun Bunnies = "bunnies"
prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle = "cuddle"
prettyVerb Hug = "hug"
prettyVerb Scare = "scare"

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Silly  = "silly"
prettyAdj Small  = "small"
prettyAdj Old  = "old"
prettyAdj Happy  = "happy"


-- | Does the sentence contain only cuddling and hugs?
-- | isNice ex2
-- |   True
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)  = False
isNice (NVN _ Cuddle _) = True
isNice (NVN _ Hug _) = True
isNice (NVN _ Scare _) = False
isNice (NV _ Scare) = False
isNice (NV _ Chase) = False
isNice (NV _ Cuddle) = True
isNice (NV _ Hug) = True
isNice (And s1 s2) = isNice s1 && isNice s2
isNice (End) = True
-- finish

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6
wordCount :: Sentence -> Int
wordCount (And l r) = wordCount l + wordCount r + 1
wordCount (NV n _) = nounCount n + 1
wordCount (NVN n1 _ n2) = nounCount n1 + 1 + nounCount n2 
wordCount (End) = 0

-- Helper Function to count the different Noun cases
nounCount :: Noun -> Int
nounCount (NP _ n) = 1 + nounCount n
nounCount (NAnd n1 n2) = nounCount n1 + 1 + nounCount n2
nounCount (_) = 1
