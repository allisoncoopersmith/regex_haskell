module Project3 where

-- Modify this file by replacing the (non-)definitions of each regular
-- expression and function with working definitions. Do not modify the types of
-- the expressions or functions, as this may prevent the grading script from
-- compiling. Similarly, do not modify the definitions of RE or ABC.


import Data.List

data RE sym
    = Never             -- match no strings
    | Empty             -- match empty string
    | Symbol sym        -- match singleton string
    | RE sym :+ RE sym  -- choice
    | RE sym :* RE sym  -- concatenation
    | Repeat (RE sym)   -- repeat zero or more times
    | Plus (RE sym)     -- repeat one or more times
    deriving (Show, Eq)

infixr 6 :+, .+.
infixr 7 :*, .*.

data ABC = A | B | C deriving (Show, Eq, Ord)


-- 1
-- Create a regular expression for the language over ABC containing strings
-- with exactly one C

one :: RE ABC
one = Repeat (Symbol A :+ Symbol B) :* Symbol C :* Repeat (Symbol A :+ Symbol B)


-- 2
-- Create a regular expression for the language over ABC containing strings
-- with an even number of A's

two :: RE ABC
two = (Repeat (Symbol C :+ Symbol B)) :+ (Repeat ((Repeat (Symbol C :+ Symbol B) :* Symbol A :* Repeat (Symbol C :+ Symbol B)) :* (Repeat (Symbol C :+ Symbol B) :* Symbol A :* Repeat (Symbol C :+ Symbol B))))


-- 3
-- Create a regular expression for the language over ABC containing strings
-- where every A is immediately followed by a B

three :: RE ABC
three = Repeat (Symbol B :+ (Symbol A :* Symbol B) :+ Symbol C)

-- 4
-- Write a function matchEmpty that returns true for regular expressions that
-- match the empty string.

matchEmpty :: RE sym -> Bool
matchEmpty Never = False
matchEmpty Empty = True
matchEmpty (Symbol sym) = False
matchEmpty (sym1 :+ sym2) = matchEmpty sym1 || matchEmpty sym2
matchEmpty (sym1 :* sym2) = matchEmpty sym1 && matchEmpty sym2
matchEmpty (Repeat sym) = True
matchEmpty (Plus sym) = matchEmpty sym



-- 5
--Returns a list of containing all symbols that a string satisfying the regex can start with.
--For example: a string satisfying the regex a*b*c can start with a b, or c so the function
--would return [a, b] for that regex. A string satisfying a+b*c can start with just a, so the
-- function would return [a] for that regex.
--
-- Note that the symbol type is completely polymorphic, so it is not possible
-- to sort the list or remove duplicates. Note also that the list must be
-- finite, even if the number of strings in the language is infinite.

firsts :: RE sym -> [sym]
firsts Never = []
firsts Empty = []
firsts (Symbol sym) = [sym]
firsts (sym1 :+ sym2) = (firsts sym1) ++ (firsts sym2)
firsts (sym1 :* sym2) = if (matchEmpty sym1) then (firsts sym1 ++ firsts sym2) else (firsts sym1)
firsts (Repeat sym) = firsts sym
firsts (Plus sym) = firsts sym



-- utilities
-- You may use matchDeriv to check your regular expressions. Note that you
-- must define matchEmpty in order for matchDeriv to work.

matchDeriv :: (Eq sym) => RE sym -> [sym] -> Bool
matchDeriv p = matchEmpty . foldl' deriv p

deriv :: (Eq sym) => RE sym -> sym -> RE sym
deriv Never      _ = Never
deriv Empty      _ = Never
deriv (Symbol s) x
    | s == x       = Empty
    | otherwise    = Never
deriv (p :+ q)   x = deriv p x .+. deriv q x
deriv (p :* q) x
    | matchEmpty p = deriv q x .+. deriv p x .*. q
    | otherwise    = deriv p x .*. q
deriv (Repeat p) x = deriv p x .*. Repeat p
deriv (Plus p)   x = deriv p x .*. Repeat p

-- Alternative forms of :+ and :* that perform some elementary simplification.
-- These reduce the size of the expressions deriv produces in many common cases.

(.+.) :: RE sym -> RE sym -> RE sym
Never .+. q     = q
p     .+. Never = p
p     .+. q     = p :+ q

(.*.) :: RE sym -> RE sym -> RE sym
Never .*. q = Never
Empty .*. q = q
p     .*. q = p :* q
