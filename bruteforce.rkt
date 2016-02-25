#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")

;;;; counterexample ;;;;;

(define S
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s0)]
             [s2 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define T
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define sigma2 (list (list 0 0) (list 0 1) (list 1 0) (list 1 1)))
(define binalpha (list 0 1))

(printf "Counterexample hint:\n")
(define ce (exists-word S T counterexample-pred))
(if (empty? ce)
(printf "No counterexample of size ~a of less was found.\n\n" 3)
(printf "The word ~a is a counterexample.\n\n" (word-value ce)))

;;;;;; prefix hint ;;;;;;
; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(define S2
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define T2
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s2)]))

(printf "Prefix hint:\n")
(define prefix (exists-word-forall-words S2 T2 bad-prefix-pred))
(if (empty? prefix)
    (printf "No prefix of length ~a or less was found.\n\n" 3)
    (printf "The prefix ~a st. p followed by any word up to length ~a will not have the desired behavior.\n\n" (word-value prefix) 3))

;;;;;;; split state hint ;;;;;;;
;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(define S3
  (automaton2 s0
              [s0 : "s0" (0 → s1)
                  (1 → s3)]
              [s1 : "s1" (0 → s3)
                  (1 → s2)]
              [s2 : "s2" (0 → s2)
                  (1 → s2)]
              [s3 : "s3" (0 → s3)
                  (1 → s3)]))
(define T3
  (automaton s0
             [s0 : (accept : #t)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #t)
                 (0 → s1)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]))

(printf "\nSplit state hint:")

(define split-state-words (exists-word-exists-word S3 T3 split-state-pred))

(if (empty? split-state-words)
    (printf "\nNo split state was found when checking words up to ~a in length.\n\n" 10)
    (printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (S3 (word-value (car split-state-words)))))

;;;;;;;; sequence of counterexamples ;;;;;;
;; sequence 1: get arbitrary number of counterexamples
;; sequence 2: get arbitrary number of counterexamples that match some pattern



;;;;;;;; small multiples ;;;;;;
;; get counterexample, then get word with good outcome that has a close edit distance from counterexample
