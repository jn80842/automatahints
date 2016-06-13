#lang racket

(require "automata.rkt")

(provide S T S2 T2 S3 T3)

;; exactly 2 0's
(define T
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

;; exactly 2 consecutive 0's
(define S
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

;; exactly 2 0's
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
;; at least 2 0's
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

;; starts with 01
(define S3-state
  (automaton2 s0
              [s0 : "s0" (0 → s1)
                  (1 → s3)]
              [s1 : "s1" (0 → s3)
                  (1 → s2)]
              [s2 : "s2" (0 → s2)
                  (1 → s2)]
              [s3 : "s3" (0 → s3)
                  (1 → s3)]))
(define S3
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))
;; contains substring 01
(define T3
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s1)
                 (1 → s2)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s2)]))

(define T3-state
  (automaton2 s0
              [s0 : "s0" (0 → s1)
                  (1 → s0)]
              [s1 : "s1" (0 → s1)
                  (1 → s2)]
              [s2 : "s2" (0 → s2)
                  (1 → s2)]))