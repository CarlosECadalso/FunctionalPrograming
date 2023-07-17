{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    runStag,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a StagShell expression by calling `eval` with the empty environment
runStag :: Expr -> Value
runStag e = eval Data.Map.empty e


-- | An interpreter for the StagShell language.
eval :: Env -> Expr -> Value
eval env (Literal v) = v
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y) -- todo
    (Error x, _) -> Error x -- todo
    (_, Error y) -> Error y -- todo
    _              -> Error "Plus" -- todo
    -- what other patterns are missing above?

eval env (Times a b) = case ((eval env a), (eval env b)) of
  (Num x, Num y) -> Num (x * y) -- todo
  (Error x, _) -> Error x -- todo
  (_, Error y) -> Error y -- todo
  _              -> Error "Times" -- todo

eval env (Equal a b) = case ((eval env a), (eval env b)) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (x, y) -> if x==y then T else F
  
eval env (Cons a b) = case ((eval env a), (eval env b)) of
  (Error x, _) -> Error x -- todo
  (_, Error y) -> Error y -- todo
  (x, y)              -> Pair x y -- todo

eval env (First a) = case ((eval env a)) of
  (Pair (Error x) y) -> Error x -- todo
  (Pair x y) -> x -- todo
  _              -> Error "First" -- todo

eval env (Rest a) = case ((eval env a)) of
  (Pair x (Error y)) -> Error y -- todo
  (Pair x y) -> y -- todo
  _              -> Error "Rest" -- todo

eval env (If c a b) = case ((eval env c)) of
  (Error cond) -> Error cond -- todo
  T -> eval env a
  _ -> eval env b

-- todo: handle Equal, Cons, First, Rest, and If
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"
-- todo: handle Lambda and App

eval env (Lambda param xpr) =  Closure param env xpr --todo fix this

eval env (App xpr arguments)  = case ((eval env xpr), (map (eval env) arguments)) of
  (Closure param env xpr, args) -> 
    let 
      new_env = auxilEnv env param args
    in 
      eval env xpr
    --(Data.Map.insert param args env)
  _  -> Error "App" -- "a" is of type Value 

auxilEnv :: Env -> [String] -> [Value] -> Env
auxilEnv env [x] [y] = Data.Map.insert x y env
auxilEnv env (x:xs) (y:ys) = Data.Map.insert x y (auxilEnv env xs ys)