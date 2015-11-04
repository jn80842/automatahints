#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

(provide same-outcome? solve-automaton-ce solve-split-state)

;; helper function: w is accepted on m1 and rejected on m2 or vice versa.
(define (same-outcome? m1 m2 w)
  (eq? (m1 w) (m2 w)))

(define (solve-automaton-ce m1 m2 alphabet k)
  (define w (word* k alphabet))
  (evaluate w (solve (assert (not (same-outcome? m1 m2 w))))))

(define (solve-split-state m1 m2 alphabet k)
  (define w (word* k alphabet))
  (define wprime (word* k alphabet))
  (m1 (evaluate w (solve (begin (assert (eq? (m1 w) (m1 wprime)))
                (assert (not (eq? (m2 w) (m2 wprime)))))))))