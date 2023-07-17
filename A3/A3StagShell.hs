{-|
Module:      A3StagShell
Description: StagShell interpreter for Lab 3
Copyright: (c) University of Toronto, 2021
               CSC324 Principles of Programming Languages, Fall 2021

This module contains the (non-CPS) version of the StagShell interpreter, along
Do not write any of your solutions in this file.
-}

import qualified Data.Map (Map, lookup, insert, empty, fromList)

------------------------------------------------------------------------------
-- Data Definitions: the (non-CPS) StagShell language
------------------------------------------------------------------------------

-- | An environment is a mapping of names to values
type Env = Data.Map.Map String Value
emptyEnv = Data.Map.empty

-- | Values in StagShell include:
data Value = T | F                -- booleans true an dfalse
           | Num Int              -- integers
           | Pair Value Value     -- pairs
           | Closure HaskellProc  -- closures, see below
           | Error String         -- errors
           deriving (Show, Eq)


-- | We represent a closure using a Haskell functions: This Haskell function
--   takes a list of arguments, and returns a single value.
--   This representation is different from the representations we used
--   in previous exercises and assignment, and is necessary for the CPS
--   transform in Part 3. We also define methods for comparing and
--   printing procedures.
data HaskellProc = Proc ([Value] -> Value)
instance Show HaskellProc where  
    show x = "[Can't Show Procedure]"
instance Eq HaskellProc where  
    x == y = False

-- | Expressions in StagShell include:
data Expr = Literal Value           -- literal values
          | Plus Expr Expr          -- builtin "plus" function
          | Times Expr Expr         -- builtin "times" function
          | Equal Expr Expr         -- builtin checks for equality
          | Cons Expr Expr          -- builtin "cons" function that creates a pair
          | First Expr              -- builtin "first" function that obtains the first element of a pair
          | Rest Expr               -- builtin "rest" function that obtains the second element of a pair
          | Var String              -- variable names
          | If Expr Expr Expr       -- if expressions
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          deriving (Eq, Show)

------------------------------------------------------------------------------
-- Interpreter
------------------------------------------------------------------------------


-- | The interpreter `eval` for StagShell, which takes an environment
--   and an expression, and returns the evaluated value.
eval :: Env -> Expr -> Value
eval env (Literal v) = v
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y)
    (Error a, _)   -> Error a
    (_, Error b)   -> Error b
    _              -> Error "Plus"
eval env (Times a b) = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x * y)
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> Error "Times"
eval env (Equal a b) = case ((eval env a), (eval env b)) of
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> if c == d then T else F
eval env (Cons a b) =  case ((eval env a), (eval env b)) of
    (Error c, _)   -> Error c
    (_, Error d)   -> Error d
    (c, d)         -> Pair c d
eval env (First expr) = case (eval env expr) of
    (Pair a b) -> a
    (Error c)  -> Error c
    _          -> Error "First"
eval env (Rest expr) = case (eval env expr) of
    (Pair a b) -> b
    (Error c)  -> Error c
    _          -> Error "Rest"
eval env (If cond expr alt) = case (eval env cond) of
    (Error c)  -> Error c
    T          -> (eval env expr)
    _          -> (eval env alt)
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a
    Nothing -> Error "Var"
eval env (Lambda params body) = Closure $ Proc $ \vargs ->
    let paramArgTuples = zip params vargs
        newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                       env
                       paramArgTuples
    in eval newEnv body
eval env (App proc args) = case (eval env proc) of
    Closure (Proc f) ->  let vargs = (map (eval env) args)
                             firstError = foldl combineError Nothing vargs
                         in case firstError of 
                             Just err -> err
                             Nothing  -> f vargs 
                             -- note that we can no longer check if
                             -- the argument length is equal to the
                             -- parameter length, since "f" is opaque
    _                -> Error "App"


-- Helper function that combines two (Maybe Value)s.
-- Returns `Nothing` if neither values are (Error _)
-- Returns the error if one of those values is (Error _)
combineError :: Maybe Value -> Value -> Maybe Value
combineError Nothing (Error v) = Just (Error v)
combineError acc     val       = acc

------------------------------------------------------------------------------
-- Environment Definition
------------------------------------------------------------------------------

-- | Create a new environment from a set of bindings (pairs of names to
--   expressions to be evaluated). There is a tricky bit of Haskell here
--   that supports *recursion* in these definitions!
--   In other words, the expression might reference names that are 
--   being defined. In order to support recursion, notice that we
--   are passing the `env` (currently being created) as the environment
--   in the call to `eval`! This is similar to how we use `letrec` in
--   Racket, but relies on Haskell's lazy evaluation.
def :: [(String, Expr)] -> Env
def bindings = 
    let env = Data.Map.fromList (map (\(n,e) -> (n, (eval env e))) bindings)
    in env

------------------------------------------------------------------------------
-- Example StagShell Programs
------------------------------------------------------------------------------

-- | Example: apply the identity function to the number 3
example1 = eval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3])

-- | Example: apply a function that returns 10 plus the second argument
--            to the arguments [1, 2]
example2 = eval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
                              [Literal $ Num 1, Literal $ Num 2])
-- | Example: if statement expression
example3 = eval emptyEnv (If (Equal (Literal F) (Literal F))
                             (Literal T)
                             (Literal F))
-- | Example: creating a function using `def`
sub1Env = def [("sub1", Lambda ["n"] (Plus (Var "n") (Literal $ Num (-1))))]
example4 = eval sub1Env (App (Var "sub1") [Literal $ Num 5])

---- | Example: a recursive factorial definition
facEnv = def [("fac", Lambda ["n"]
                (If (Equal (Var "n") (Literal $ Num 0))
                    (Literal $ Num 1)
                    (Times (Var "n") (App (Var "fac")
                       [(Plus (Var "n") (Literal $ Num (-1)))]))))]
example5 = eval facEnv (App (Var "fac") [Literal $ Num 5])

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    print example1
    print example2
    print example3
    print example4
    print example5
