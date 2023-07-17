{-|
Module: A2StarterTests
Description: Starter Tests for A2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}

module A2StartTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A2 (runStag, eval)
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)

--Some simple tests to get you started--

prop_testLiteralNumber:: Int -> Property
prop_testLiteralNumber x = label "literal numbers" $
    let expr = (Literal $ Num x)
        result = runStag expr
    in result == Num x

prop_testAddition:: Int -> Int -> Property
prop_testAddition x y = label "addition tests" $
    let expr = Plus (Literal $ Num x) (Literal $ Num y)
        result = runStag expr
    in result == Num (x + y)

prop_testMultiplication:: Int -> Int -> Property
prop_testMultiplication x y = label "multiplication tests" $
        let expr = Times (Literal $ Num x) (Literal $ Num y)
            result = runStag expr
        in result == Num (x * y)

prop_testEquality1:: Int -> Property
prop_testEquality1 x = label "equality tests 1" $
        let expr = Equal (Literal $ Num x) (Literal $ Num x)
            result = runStag expr
        in result == T

prop_testEquality2:: Int -> Int -> Property
prop_testEquality2 x y = label "equality tests 2" $
        let expr = Equal (Literal $ Num x) (Literal $ Num y)
            result = runStag expr
        in if x == y then result == T else result == F

prop_testFirst:: Int -> Int -> Property
prop_testFirst x y = label "first tests" $
        let expr = First (Cons (Literal $ Num x) (Literal $ Num y))
            result = runStag expr
        in result == Num x

prop_testLast:: Int -> Int -> Property
prop_testLast x y = label "last tests" $
        let expr = Rest (Cons (Literal $ Num x) (Literal $ Num y))
            result = runStag expr
        in result == Num y

prop_testIf1:: Int -> Property
prop_testIf1 x = label "if tests 1" $
        let expr = If (Equal (Literal $ Num x) (Literal $ Num x)) (Plus (Literal $ Num x) (Literal $ Num 1)) (Plus (Literal $ Num x) (Literal $ Num 2))
            result = runStag expr
        in result == Num (x + 1)

prop_testIf2:: Int -> Property
prop_testIf2 x = label "if tests 1" $
        let expr = If (Equal (Literal $ Num (x+1)) (Literal $ Num x)) (Plus (Literal $ Num x) (Literal $ Num 1)) (Plus (Literal $ Num x) (Literal $ Num 2))
            result = runStag expr
        in result == Num (x + 2)


prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "identifier error" $
  let expr = Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal T))
      result = runStag expr
  in result == Error "Times"

prop_testBasicIdentifier2 :: Property
prop_testBasicIdentifier2 = label "identifier error" $
  let expr = Plus (Literal $ Num 3) (Literal T)
      result = runStag expr
  in result == Error "Plus"

prop_testBasicIdentifier3 :: Property
prop_testBasicIdentifier3 = label "identifier error" $
  let expr = Cons (Literal $ Num 1) (Times (Literal $ Num 3) (Literal T))
      result = runStag expr
  in result == Error "Times"

prop_testBasicIdentifier4 :: Property
prop_testBasicIdentifier4 = label "identifier error" $
  let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
      result = runStag (App fnExpr1 [])
  in result == Error "App"

prop_testFunctionApplication2 :: Int -> Property
prop_testFunctionApplication2 x = label "function application 2" $
      let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
          result = runStag (App fnExpr1 [Literal (Num x)])
      in result == Num (x + 1)

prop_testFunctionApplication :: Int -> Int -> Property
prop_testFunctionApplication x y = label "function application" $
    let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
        fnExpr2 = Lambda ["a", "b"] (Times (Var "a") (App fnExpr1 [(Var "b")]))
        result = runStag (App fnExpr2 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * (y + 1))


-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testLiteralNumber
    quickCheck prop_testAddition
    quickCheck prop_testMultiplication
    quickCheck prop_testEquality1
    quickCheck prop_testEquality2
    quickCheck prop_testFirst
    quickCheck prop_testLast
    quickCheck prop_testIf1
    quickCheck prop_testIf2
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testBasicIdentifier2
    quickCheck prop_testBasicIdentifier3
    quickCheck prop_testBasicIdentifier4
    quickCheck prop_testFunctionApplication2
    quickCheck prop_testFunctionApplication
