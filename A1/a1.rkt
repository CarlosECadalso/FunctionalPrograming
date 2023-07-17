#lang racket #| * CSC324H5 Fall 2021: Assignment 1 * |#
#|
Module:        a1
Description:   Assignment 1: Checking for Tail Recursion
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; This specifies which functions this module exports. Don't change this!
(provide is-tail-recursive)

; Import the testing library
(module+ test
  (require rackunit))


#|
(is-tail-recursive def) -> boolean? 
  def: (and/or symbol? number?) 
    A function definition that follows the grammar for <def> in the handout.
    You may assume that the function defined in def is recursive.

  Returns whether the definition is tail recursive
|#
(define (is-tail-recursive def)
  (let* ([fname (first (second def))]
         [body  (third def)])
    (all-tail-position fname body #f))) ; TODO: replace (void) with your code

(define/match (all-tail-position fname definition already-found)
  [(fname '() already-found)
   #f]
  [(fname (cons x xs) already-found)
    (cond
      [(equal? x 'if) (let* ([conditional (second definition)]
         [true-def  (third definition)] [false-def  (list-ref definition 3)])
    (deal-with-if fname conditional true-def false-def already-found))]

      [(equal? x 'let*) (let* ([conditional (second definition)]
         [actual-def  (third definition)])
    (deal-with-let fname conditional actual-def already-found))]

      [else (deal-with-list fname definition already-found)] )
  ]
  [(fname single-value already-found)
    (already-found)]
 )

(define (deal-with-list fname definition already-found)
  (if (has-call fname definition)
           (if (is-call fname definition) (deal-with-function fname definition already-found)
               (if (is-other-function fname definition) (not (has-call fname (list-tail definition 1)))
               (map (all-tail-position fname (list-tail definition 1) already-found) (definition))
               )
               )
                
           #f)
)


(define (is-other-function fname definition)
  (if (<= (length definition) 1) #f
       (not (equal? (first definition) fname))
   )
)


(define/match (deal-with-function fname definition already-found)
  [(fname (cons x xs) #f)
   (if (is-call fname definition)
       (not (has-call fname xs)) (all-tail-position fname xs #f))]
  [(fname (cons x xs) #t)
   (if (has-call fname definition)
       #f (all-tail-position fname xs #t))]
)

(define (deal-with-if fname conditional true-def false-def already-found)
  (if (has-call fname conditional) #f (or (all-tail-position fname true-def already-found)  (all-tail-position fname false-def already-found)))
)

(define (deal-with-let fname conditional definition already-found)
  (if (has-call fname conditional) #f (all-tail-position fname definition already-found))
)

(define/match (has-call fname definition)
  [(fname '())
   #f]
  [(fname (cons x xs))
   (if (< (length definition) 1) #f
       (if (equal? (first definition) fname)
       #t (or (has-call fname (first definition)) (has-call fname (list-tail definition 1)))
       )
   )]
  [(fname definition)
   #f]
 )

(define/match (is-call fname definition)
  [(fname '())
   #f]
  [(fname (cons x xs))
   (if (<= (length definition) 1) #f
       (equal? (first definition) fname)
   )]
  [(fname definition)
   (#f)]
 )

; You can write helper functions freely
(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Simple test for tail recursion"        ; Test label
               (is-tail-recursive '(def (f x) (f x)))  ; Actual value
               #t)                                     ; Expected value
  (test-equal? "Simple test for tail recursion"       
               (all-tail-position 'f '(f a) #t) 
               #f)
  (test-equal? "Simple test for tail recursion"       
               (is-tail-recursive '(def (f x) (f (f a)))) 
               #f)
  (test-equal? "Simple test for tail recursion"       
               (is-tail-recursive '(def (f x) (g (f a)))) 
               #f)
  (test-equal? "Simple test for tail recursion"       
               (is-tail-recursive '(def (f x) (f (g a)))) 
               #t)
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) x x)))
               #f)
  (test-equal? "Recursive call in let* definition"
               (is-tail-recursive '(def (f x) (let* ([a (f x)]) (g a))))
               #f)
  (test-equal? "Recursive call in let* body"
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (f a))))
               #t)

  ; TODO: Write more tests. Testing is an important part of programming,
  ; so ou and your partner must write your own tests. Do not share your
  ; tests with anyone else.
)

