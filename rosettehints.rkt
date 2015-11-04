#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")
(require "rosettehintmethods.rkt")

(provide same-outcome? solve-automaton-ce solve-split-state)

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



(printf "Counterexample hint:\n")


(printf "The word ~a is a counterexample.\n\n" (solve-automaton-ce S T (list 0 1) 3))

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
(define (prefixer w)
  (define p (list (??) (??) (??)))
         (append p w))

(define (prefix-check m1 m2 prefixer w)
  (assert (not (same-outcome? m1 m2 (prefixer w)))))

(define ww (word* 3 '(0 1)))
(define binding
  (synthesize #:forall (list ww)
              #:guarantee (prefix-check S2 T2 prefixer ww)))
(printf "This synthesized function contains a prefix p st. p followed by any word up to length k will not have the desired behavior.\n\n")
(print-forms binding)

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

(printf "\nSplit state hint:\n")

(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (solve-split-state S3 T3 (list 0 1) 3))
