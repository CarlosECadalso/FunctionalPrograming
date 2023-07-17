#lang racket #| * CSC324H5 Fall 2021: Assignment 4 * |#
#|
Module:        a4
Description:   Assignment 4: Theorem Proving in miniKanren
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; Do not add additional imports
(require "mk.rkt")

; This specifies which functions this module exports. Don't change this!
(provide proof?
         my-proof
         proofo
         prove)

; Import the testing library
(module+ test
  (require rackunit))

;-------------------------------------------------------------------------------
; Task 1: Using a proof checker
;-------------------------------------------------------------------------------

#|
(proof? prop proof) -> bool?
  prop: (and/or list? symbol?)
    A proposition that we wish to prove
  proof: list?
    A proof for that proposition

  Returns a boolean describing whether `proof` is a valid proof of `prop`
|#
(define (proof? prop proof)
  (equal? (proof/asmpt proof '()) ; check what proposition 'proof' proves
          prop))                  ; and check if it is equal to 'prop'

#|
(proof/asmpt proof asmpts) -> (and/or bool? list?)
  proof: list?
    A proof for that proposition
  asmpt: list?
    A list of currently valid assumptions

  Return either the proposition that a proof is proving given the assumption,
  or #f is the proof is invalid.
|#
(define/match (proof/asmpt proof asmpt)
  [((list 'use p) asmpt)
   (if (member p asmpt) p #f)]
  [((list 'assume hypo subproof) asmpt)
   (list hypo '-> (proof/asmpt subproof (cons hypo asmpt)))]
  [((list 'modus-ponens subpf1 subpf2) asmpt)
   (let* ([prop2 (proof/asmpt subpf2 asmpt)])
     (match prop2            ; "match" is like "case ... of" in Haskell
       [(list hypo '-> conc) ; and does pattern matching on "prop2"
        (if (equal? (proof/asmpt subpf1 asmpt) hypo)
            conc
            #f)]
       [prop2 #f]))])


#|
my-theorem: A theorem that you will prove.
|#
(define my-theorem '((A -> (B -> C)) -> (B -> (A -> C))))

#|
my-proof: A proof for my-theorem: (proof? my-proof my-theorem)
should return true.
|#
(define my-proof '(assume (A -> (B -> C))  (assume B (assume A (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C))))))))
  )



(module+ test
  (test-equal? "my-proof proves my-theorem"
               (proof? my-theorem my-proof)
               #t)

(test-equal? "Example 1 from the handout"
               (proof? '(A -> A)
                       '(assume A (use A)))
               #t)
(test-equal? "Example 2 from the handout"
               (proof? '(A -> ((A -> B) -> B))
                       '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B))))))
               #t)
(test-equal? "Example 3 from the handout"
               (proof? '((C -> D) -> (C -> D))
                       '(assume (C -> D) (assume C (modus-ponens (use C) (use (C -> D))))))
               #t)
(test-equal? "Example 4 from the handout"
               (proof? '((C -> D) -> (C -> D))
                       '(assume (C -> D) (use (C -> D))))
               #t)
(test-equal? "Example 5 from the handout"
               (proof? '((A -> (B -> C)) -> (B -> (A -> C)))
                       '(assume (A -> (B -> C)) (assume B (assume A (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C)))))))))
               #t)
)

;-------------------------------------------------------------------------------
; Task 2: A Proof Checking Relation
;-------------------------------------------------------------------------------

#|
(proofo prop proof)
  prop:  A proposition following the grammar specified in the handout
  proof: A proof following the grammar specified in the handout

  The relational form of the `proof?` function. Succeeds when "proof"
  is a correct proof of the proposition "prop".
|#
(define (proofo prop proof)
  (proof-helpero prop proof '()))

#|
(proof-helpero prop proof assmpts)
  prop:    A proposition following the grammar specified in the handout
  proof:   A proof following the grammar specified in the handout
  assmpts: A list of assumptions propositions

  The relational form of the `proof/asmpt` function. Succeeds when "proof"
  is a correct proof of the proposition "prop" given the list of
  assumptions "assmpts".
|#
(define (proof-helpero prop proof assmpts)
    (conde ((fresh (p)
                  (membero p assmpts)
                  (== (list 'use p) proof)
                  (== p prop)))
           ((fresh (assmpt hypo subproof subprop1 subprop2)
                  (== (list 'assume hypo subproof) proof)
                  (membero hypo prop)
                  (== (list subprop1 '-> subprop2) prop)
                  (== hypo subprop1)
                  (proof-helpero subprop2 subproof (cons hypo assmpts))))
           ((fresh (subpf1 subpf2 subpfi hypo conc)
                  (== (list 'modus-ponens subpf1 subpf2) proof)
                  (== (list 'use subpfi) subpf2)
                  (== (list hypo '-> conc) subpfi)
                  (membero subpfi assmpts)
                  (proof-helpero hypo subpf1 assmpts)
                  (== prop conc))))
)

#|
(membero x lst)
  x:   A value
  lst: A list

  The relational form of the `member` function. Succeeds when "x"
  is an element of the list "lst". You may use this relation as a helper
  in your implementation of "proof-helpero".
|#
(define (membero x lst)
  (fresh (first rest)
    (== lst (cons first rest))
    (conde ((== first x))
           ((=/= first x)
            (membero x rest)))))


; Uncomment these to test your proofo relation
(module+ test
  (test-equal? "Example 1 from the handout"
               (run 1 (q) (proofo '(A -> A) '(assume A (use A))))
               '(_.0))
  (test-equal? "Example 2 from the handout"
               (run 1 (q) (proofo '(A -> ((A -> B) -> B))
                                  '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B)))))))
               '(_.0))
  (test-equal? "Example of an incorrect proof"
               (run 1 (q) (proofo '(A -> ((A -> B) -> B))
                                  '(assume (A -> B) (assume A (modus-ponens (use A) (use (A -> B)))))))
               '())
  ; Write more tests here
)

;-------------------------------------------------------------------------------
; Task 3: A Theorem Prover
;-------------------------------------------------------------------------------

#|
(prove prop)
  prop:  A proposition following the grammar specified in the handout

  Returns a proof of the proposition, if a proof exists. Otherwise,
  this function returns `#f` or may fail to terminate.
|#
(define (prove prop)
  (let* ([proof (first (run 1 (proof) (proofo prop proof)))])
    (if (proof? prop proof) proof #f)))



; Uncomment these to test your proofo relation
(module+ test
     (test-equal? "Example 1 from the handout"
               (prove '(A -> A))
               '(assume A (use A)))
     (test-equal? "Example 2 from the handout"
               (prove '(A -> ((A -> B) -> B)))
               '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B))))))
     (test-equal? "Example 3 from the handout"
               (prove '((C -> D) -> (C -> D)))
               '(assume (C -> D) (use (C -> D))))
  #;(test-equal? "Example 4 from the handout"
               (prove '((A -> (B -> C)) -> (B -> (A -> C))))
               '(assume (A -> (B -> C)) (assume B (assume A (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C)))))))))
  (test-equal? "Example 5 from the handout"
                (prove '(A -> (B -> A))
                       )
               '(assume A (assume B (use A))))
  #;(test-equal? "Example 6 from the handout"
                (prove '(A -> (B -> B))
                       )
               '(assume A (assume B (use B))))
  (test-equal? "Example 7 from the handout"
                (prove '((A -> B) -> ((B -> C) -> (A -> C)))                       )
               '(assume (A -> B) (assume (B -> C) (assume A (modus-ponens (modus-ponens (use A) (use (A -> B))) (use (B -> C)))))))
  #;(test-equal? "Example 8 from the handout"
                (prove '(((A -> (B -> C)) -> C) -> ((B -> (A -> C)) -> C))
                       )
               #t)
    
)
  ; Be careful when you write new tests, since they may be slow
                       
