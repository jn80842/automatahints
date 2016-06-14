#lang racket

(require "automata.rkt")
(require "descriptionhelpers.rkt")

(provide (all-defined-out))

(define at-least-k-s (λ (k s)
                       (language (list 0 1) (curry greater-eq-k k s) "Words with at least ~a occurrences of ~a")))
(define at-most-k-s (λ (k s)
                      (language (list 0 1) (curry less-eq-k k s) "Words with at most ~a occurrences of ~a")))
(define exactly-k-s (λ (k s)
                      (language (list 0 1) (curry exactly-k k s) "Words with exactly ~a occurrences of ~a")))