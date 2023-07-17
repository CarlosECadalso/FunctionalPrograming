{-|
Module: A3StarterTests
Description: Starter Tests for A3
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}

module A3StarterTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A3 (cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
           cpsMergeSort, cpsSplit, cpsMerge, cpsEval)
import A3Types (Env, emptyEnv, Value(..), HaskellProc(..), Expr(..))

-- | Warmup Task tests

prop_testFactorial :: Bool
prop_testFactorial = cpsFactorial 3 id == 6
prop_testFibonacci :: Bool
prop_testFibonacci = (cpsFibonacci 6 id) == 8

prop_cpsLength :: Bool
prop_cpsLength = (cpsLength [1,2,3] id) == 3
prop_cpsMap :: Bool
prop_cpsMap = (cpsMap (2*) [1,2,3,4,5] id) == [2,4,6,8,10]

prop_cpsMergeSort :: Bool
prop_cpsMergeSort = (cpsMergeSort [1,2,4,3] id) == [1,2,3,4]

-- | Example: apply the identity function to the number 3
example1 = cpsEval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3]) id

-- | Example: apply a function that returns 10 plus the second argument
--            to the arguments [1, 2]
example2 = cpsEval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
                              [Literal $ Num 1, Literal $ Num 2]) id
-- | Example: if expression
example3 = cpsEval emptyEnv (If (Equal (Literal F) (Literal F))
                             (Literal T)
                             (Literal F)) id
-- | Example: shift expression
example4 = (cpsEval emptyEnv
                    (Plus (Literal $ Num 2)
                          (Shift "d" 
                              (Plus
                                  (App (Var "d") [Literal $ Num 5])
                                  (App (Var "d") [Literal $ Num 10]))
                     ))
                     id)

example5 = (cpsEval emptyEnv
                    (Plus (Literal $ Num 1)
                          (Shift "d" 
                                (Plus (App (Var "d") [Literal $ Num 1]) 
                                    (App (Var "d") [Literal $ Num 0]))
                          )
                          )
                     id)

-- (cpsEval emptyEnv (Plus (Literal $ Num 2) (Shift "d" (Plus (App (Var "d") [App (Var "d") [Literal $ Num 5]]) (App (Var "d") [Literal $ Num 10])) )) id)


-- | Example: shift expression


prop_cpsEvalExample1 :: Bool
prop_cpsEvalExample1 = example1 == Num 3
prop_cpsEvalExample2 :: Bool
prop_cpsEvalExample2 = example2 == Num 12
prop_cpsEvalExample3 :: Bool
prop_cpsEvalExample3 = example3 == T
prop_cpsEvalExample4 :: Bool
prop_cpsEvalExample4 = example4 == Num 19
prop_cpsEvalExample5 :: Bool
prop_cpsEvalExample5 = example5 == Num 21

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
--
main :: IO ()
main = do
    quickCheck prop_testFactorial
    quickCheck prop_testFibonacci
    quickCheck prop_cpsLength
    quickCheck prop_cpsMap
    quickCheck prop_cpsMergeSort
    quickCheck prop_cpsEvalExample1
    quickCheck prop_cpsEvalExample2
    quickCheck prop_cpsEvalExample3
    print example4
    quickCheck prop_cpsEvalExample4
    print example5
    quickCheck prop_cpsEvalExample5
