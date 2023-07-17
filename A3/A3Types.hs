{-|
Module:      A3Types
Description: Types for Assignment 3
Copyright: (c) University of Toronto, 2021
               CSC324 Principles of Programming Languages, Fall 2021

This module provides the public types required for A3
You should review the data type Expr carefully, but do not
change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}

module A3Types (Env, emptyEnv, Value(..), HaskellProc(..), Expr(..)) where

import qualified Data.Map as Map

------------------------------------------------------------------------------
-- Data Definitions
------------------------------------------------------------------------------

-- | An environment is a mapping of names to values
type Env = Map.Map String Value
emptyEnv = Map.empty

-- | Like before, Values in StagShell include:
data Value = T 
           | F                -- booleans true an dfalse
           | Num Int              -- integers
           | Pair Value Value     -- pairs
           | Closure HaskellProc  -- closures, see below
           | Error String         -- errors    
          deriving (Eq, Show)

-- | We will represent StagShell closures using Haskell functions.
--   To complete the CPS transform, the Haskell function also need
--   to be written in CPS.
data HaskellProc = Proc ([Value] -> (Value -> Value) -> Value)
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
          | If Expr Expr Expr       -- if statements
          | Var String              -- variable names
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          | Shift String Expr       -- identical to `shift` in Racket
          | Reset Expr              -- identical to `reset` in Racket
          deriving (Eq, Show)

