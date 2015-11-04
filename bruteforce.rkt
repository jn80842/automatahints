#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")

;;;; counterexample ;;;;;

(define S
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s0)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define T
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define sigma2 (list (list 0 0) (list 0 1) (list 1 0) (list 1 1)))
(define binalpha (list 0 1))





(printf "Counterexample hint:\n")
(printf "The word ~a is a counterexample.\n\n" (find-counterexample S T (list 0 1) 20))

;;;;;; prefix hint ;;;;;;
; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(define S2
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define T2
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s2)
                 (1 → s2)]))


(printf "Prefix hint:\n")
(printf "The prefix ~a st. p followed by any word up to length k will not have the desired behavior.\n\n" (find-failing-prefix S2 T2 binalpha 3))

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
             [s0 : accept (0 → s1)
                 (1 → s0)]
             [s1 : accept (0 → s1)
                 (1 → s2)]
             [s2 : (0 → s2)
                 (1 → s2)]))



(define split-state-hint (find-split-state S3 T3 binalpha 10))
(printf "\nSplit state hint:")
(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (car split-state-hint))
                                                                                
                  

    