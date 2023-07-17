{-|
 -
Module:      A3
Description: Assignment 3
Copyright: (c) University of Toronto, 2021
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!

module A3 (
    -- Warmup Task
    cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
    cpsMergeSort, cpsSplit, cpsMerge,
    -- Main Task
    cpsEval
) where

-- You *may not* add imports from Data.Map, or any other imports
import qualified Data.Map (Map, lookup, insert, empty, fromList)
import A3Types (Env, emptyEnv, Value(..), HaskellProc(..), Expr(..))
import Debug.Trace


------------------------------------------------------------------------------
-- * Warmup Task. CPS Transforming Haskell Functions *
------------------------------------------------------------------------------

-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = cpsFactorial (n - 1) (\res -> k (n * res))



-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)

-- fibonacci :: Int -> Int
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0
cpsFibonacci 1 k = k 1
cpsFibonacci n k = cpsFibonacci (n-1) (\f1 ->
                    cpsFibonacci (n-2) (\f2 ->
                        k (f1 + f2)))

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0
cpsLength (x : xs) k = cpsLength xs (\res -> k (1 + res))


-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f (x : xs) k = cpsMap f xs (\res -> k (f x : res))

------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- is placed in one sub-list, and all list elements in odd indicies
-- is placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort [] k = k []
cpsMergeSort [x] k = k [x]
cpsMergeSort lst k = cpsSplit lst (\splitPair -> cpsMerge (fst splitPair) (snd splitPair) k)
    -- (cpsMerge (fst listPair) (snd listPair))

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit [] k = k ([], [])
cpsSplit [x] k = k ([x], [])
cpsSplit (x : y : xs) k = cpsSplit xs (\res -> k (x : fst res , y : snd res))

-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge [] [] k = k []
cpsMerge [] lst2 k = k lst2
cpsMerge lst1 [] k = k lst1
cpsMerge (x : xs) (y : ys) k = if x < y
    then cpsMerge xs ys (\res -> k (x:y:res))
    else cpsMerge xs ys (\res -> k (y:x:res))

------------------------------------------------------------------------------
-- * Main Task. CPS Transforming The StagShell Interpreter *
------------------------------------------------------------------------------

-- | A CPS interpreter `eval` for StagShell, which takes an environment,
--   an expression, and a continuation, and calls the continuation with
--   the evaluated value.
--   Notice that the type signature of `eval` is less general compared to
--   usual, i.e. it is not:
--      Env -> Expr -> (Value -> r) -> r
--   This restriction on the type of the continuation makes it easier
--   to check for errors.
cpsEval :: Env -> Expr -> (Value -> Value) -> Value
cpsEval env (Literal v) k = k v

cpsEval env (Plus a b) k =
    cpsEval env a $ \valA ->
    cpsEval env b $ \valB ->
        case (valA, valB) of
            (Num x, Num y) -> k (Num (x + y))
            (Error a, _)   -> Error a
            (_, Error b)   -> Error b
            _              -> Error "Plus"
    

cpsEval env (Times a b) k = 
    cpsEval env a $ \valA ->
    cpsEval env b $ \valB ->
        case (valA, valB) of
            (Num x, Num y) -> k (Num (x * y))
            (Error a, _)   -> Error a
            (_, Error b)   -> Error b
            _              -> Error "Times"
    



cpsEval env (Equal a b) k = 
    cpsEval env a $ \valA ->
    cpsEval env b $ \valB ->
        case (valA, valB) of
            (Error c, _)   -> Error c
            (_, Error d)   -> Error d
            (c, d)         -> if c == d then k T else k F
    


cpsEval env (Cons a b) k = 
    cpsEval env a $ \valA ->
    cpsEval env b $ \valB ->
        case (valA, valB) of
            (Error c, _)   -> Error c
            (_, Error d)   -> Error d
            (c, d)         -> Pair c d
    


cpsEval env (First expr) k = 
    cpsEval env expr $ \valA ->
    case valA of
        Pair a b -> a
        Error c  -> Error c
        _        -> Error "First"


cpsEval env (Rest expr) k = 
    cpsEval env expr $ \valA ->
    case valA of
        Pair a b -> b
        Error c  -> Error c
        _        -> Error "Rest"
        


cpsEval env (If cond expr alt) k = 
    cpsEval env cond $ \cond2 ->
    case cond2 of
    Error c  -> Error c
    T          -> cpsEval env expr k
    _          -> cpsEval env alt k
    


cpsEval env (Var name) k = case Data.Map.lookup name env of
    Just a  -> k a
    Nothing -> Error "Var"    

cpsEval env (Lambda params body) k = k $ Closure $ Proc (\values k2 ->
    let paramArgTuples = zip params values
        newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                       env
                       paramArgTuples
    in cpsEval newEnv body k2)
    -- Question: Is the continuation of the function defined by this lamba a separate parameter

cpsEval env (App proc args) k = cpsEval env proc (\proc2 -> let vargs = cpsEvalList env args k in
                case proc2 of
                        Closure (Proc f) -> let firstError = cpsCombineError env vargs k in
                            case firstError of
                                Error c -> Error c
                                F ->  f vargs k
                                _ -> Error "App"
                            -- Question: Is the continuation of the function defined by lamba a separate parameter or not?
                            -- Meaning: Is it "f vargs k" or "k f vargs"
                        Error c -> Error c
                        _ -> Error "App")

cpsEval env (Shift name expr) k =
    cpsEval (
        Data.Map.insert name (
            Closure $ Proc (\[x] k -> k x)
        ) env
    ) expr id
                                    
    -- Closure $ Proc $ \[x] k -> k (Num 0)

    -- Closure $ Proc $ \a k -> k $ head a
                                    
    -- let f k a = traceShow (k, a) in cpsEval (Data.Map.insert name (Closure $ Proc $ \[a] k -> k a) env) expr id

                                    -- I don't understand how the continuation is being passed.
                                    -- Is k the continuation?
                                    -- If yes: how are we supposed to bind k, which is a (Value -> Value) into a Closure
                                    
                                    -- I don't fully understand how HaskellProc work

                                    -- I don't understand how are we supposed to find the reset in the continuation

                                    -- I don't understand how we are supposed to evaluate the expression
                                    -- Are we supposed to pass the contiuation down?
                                    -- If not: What do we pass as the parameter k of expr



cpsEval env (Reset expr) k = let val = cpsEval env expr id in case val of 
                                            Error c -> Error c
                                            _ -> k val

-- Returns F if no values are (Error _)
-- Returns the first error if one of those values is (Error _)
cpsCombineError :: Env -> [Value] -> (Value -> Value) -> Value 
cpsCombineError env [] k = F
cpsCombineError env (x : xs) k = case x of
                                    Error c -> k (Error c) 
                                    _ -> cpsCombineError env xs k

cpsEvalList :: Env -> [Expr] -> (Value -> Value) -> [Value]
cpsEvalList env [] k = []
cpsEvalList env [a] k = [cpsEval env a k]
cpsEvalList env (x : xs) k = cpsEval env x k : cpsEvalList env xs k
