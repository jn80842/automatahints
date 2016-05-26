#lang racket

(require racket/generator)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")

;; describe DFA (counterexamples as accepting words that all have more than k occurrences of '0'
;; in other words, synthesize a description of the full counterexample language using a single predicate

;; the language Sigma* where Sigma = {0, 1} and words are up to 8 symbols in length
(define sigmastar (words (list 0 1) 8))

;; we want to be able to compose languages. 
(define (dfa-lang lang M)
  (generator ()
             (begin
               (for ([w (in-producer lang)]
                     #:break (empty? w)
                     #:when (and (not (empty? w)) (M (word-value w))))
                 (yield w)))
             null))

;; language of counterexamples for S2 and T2
;; note this is NOT nice composition
(define (ce-lang M1 M2)
  (generator ()
             (begin
               (for ([w (in-producer (words (alphabet M1) 8))]
                     #:break (empty? w)
                     #:when (not (eq? (M1 (word-value w)) (M2 (word-value w)))))
                 (yield w)))
             null))

;; predicate for greater-than number of occurrences
(define (greater-than-k k symbol w)
  (< k (count (λ (s) (eq? symbol s)) w)))
                 
;; let's curry the predicate with the values we know will work
;; we need our procedure to take 2 DFAs as args for now since we're recycling forall-words
(define curried-greater-than
  (λ (M1 M2 w)
    (greater-than-k 2 0 w)))

;; we can use the forall operator to check that our curried predicate is true
;; we get back null, which means forall couldn't find a counterexample
(define test-curried (forall-words-flat (ce-lang S2 T2) S2 T2 curried-greater-than))

;; let's search for k
;; since sigmastar is limited to words of length 8 or less, k can't be greater than 8
(define (k-search lang M1 M2)
  (for/first ([k (in-range 1 9)]
              #:when (empty? (forall-words-flat lang M1 M2
                                                (λ (M1 M2 w) (greater-than-k k 0 w)))))
    k))

(print "All counterexample words have ")
(print (k-search (ce-lang S2 T2) S2 T2))
(println " or more 0s.")

;; not a tight enough bound -- we want this predicate to be true for all counterexample
;; and false for all correctly classified words
(define (correct-lang M1 M2)
  (generator ()
             (begin
               (for ([w (in-producer (words (alphabet M1) 8))]
                     #:break (empty? w)
                     #:when (eq? (M1 (word-value w)) (M2 (word-value w))))
                 (yield w)))
             null))

(define (better-k-search lang lang2 M1 M2)
  (for/first ([k (in-range 1 9)]
              #:when (and (empty? (forall-words-flat lang M1 M2
                                                (λ (M1 M2 w) (greater-than-k k 0 w))))
                          (empty? (forall-words-flat lang2 M1 M2
                                                          (λ (M1 M2 w) (not (greater-than-k k 0 w)))))))
    k))

(println (better-k-search (ce-lang S2 T2) (correct-lang S2 T2) S2 T2))

;; search for k and symbol
(define (k-and-symbol-search lang lang2 M1 M2)
  (for*/first ([x (alphabet M1)]
              [k (in-range 1 9)]
              #:when (and (empty? (forall-words-flat lang M1 M2
                                                     (λ (M1 M2 w) (greater-than-k k x w))))
                          (empty? (forall-words-flat lang2 M1 M2
                                                     (λ (M1 M2 w) (not (greater-than-k k x w)))))))
    (cons k x)))

