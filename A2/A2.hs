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
eval env (Plus a b)  = case (eval env a, eval env b) of
    (Num x, Num y) -> Num (x + y) 
    (Error x, _) -> Error x 
    (_, Error y) -> Error y 
    _              -> Error "Plus" 

eval env (Times a b) = case (eval env a, eval env b) of
  (Num x, Num y) -> Num (x * y) 
  (Error x, _) -> Error x 
  (_, Error y) -> Error y 
  _              -> Error "Times" 

eval env (Equal a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x
  (_, Error y) -> Error y
  (x, y) -> if x==y then T else F
  
eval env (Cons a b) = case (eval env a, eval env b) of
  (Error x, _) -> Error x 
  (_, Error y) -> Error y 
  (x, y)              -> Pair x y 

eval env (First a) = case eval env a of
  (Pair (Error x) y) -> Error x 
  (Pair x y) -> x 
  _              -> Error "First" 

eval env (Rest a) = case eval env a of
  (Pair x (Error y)) -> Error y 
  (Pair x y) -> y 
  _              -> Error "Rest" 

eval env (If c a b) = case eval env c of
  (Error cond) -> Error cond 
  T -> eval env a
  _ -> eval env b

eval env (Var name)  = case Data.Map.lookup name env of
    Just a  -> a -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"

eval env (Lambda param xpr) =  Closure param env xpr 

eval env (App xpr arguments)  = case (eval env xpr, map (eval env) arguments) of
  (Closure param env xpr, args) -> 
    let 
      new_env = auxilEnv env param args
    in
      if length param == length args
        then eval new_env xpr
        else Error "App"
    --(Data.Map.insert param args env)
  _  -> Error "App" -- xpr does not evaluate to closure 

auxilEnv :: Env -> [String] -> [Value] -> Env
auxilEnv env [] [y] = Data.Map.insert "null" (Num 0) env
auxilEnv env [x] [] = Data.Map.insert "null" (Num 0) env
auxilEnv env [x] [y] = Data.Map.insert x y env
auxilEnv env (x:xs) (y:ys) = auxilEnv (Data.Map.insert x y env) xs ys 