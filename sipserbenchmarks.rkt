#lang racket

(require "automata.rkt")
(provide (all-defined-out))

;; Sipser 1.6a
;; { w | w starts with '1' and ends with '0' }

(define sipser16a
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s3)
                 (1 → s1)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s1)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

;; after getting to s2, both 0 and 1 go back to
;; "waiting to see a final zero" state
(define studentsipser16a
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s3)
                 (1 → s1)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s1)
                 (1 → s1)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define sipserp134
  (automaton s0
             [s0 : (accept : #t)
                 ('000 → s0)
                 ('001 → s1)
                 ('010 → s2)
                 ('011 → s0)
                 ('100 → s2)
                 ('101 → s0)
                 ('110 → s2)
                 ('111 → s2)]
             [s1 : (accept : #f)
                 ('000 → s2)
                 ('001 → s1)
                 ('010 → s1)
                 ('011 → s2)
                 ('100 → s1)
                 ('101 → s2)
                 ('110 → s0)
                 ('111 → s1)]
             [s2 : (accept : #f)
                 ('000 → s2)
                 ('001 → s2)
                 ('010 → s2)
                 ('011 → s2)
                 ('100 → s2)
                 ('101 → s2)
                 ('110 → s2)
                 ('111 → s2)]))

(define studentp134
  (automaton s0
             [s0 : (accept : #t)
                 ('000 → s0)
                 ('001 → s1)
                 ('010 → s2)
                 ('011 → s0)
                 ('100 → s2)
                 ('101 → s0)
                 ('110 → s2)
                 ('111 → s2)]
             [s1 : (accept : #f)
                 ('000 → s2)
                 ('001 → s1)
                 ('010 → s1)
                 ('011 → s2)
                 ('100 → s1)
                 ('101 → s2)
                 ('110 → s0)
                 ('111 → s2)]
             [s2 : (accept : #f)
                 ('000 → s2)
                 ('001 → s2)
                 ('010 → s2)
                 ('011 → s2)
                 ('100 → s2)
                 ('101 → s2)
                 ('110 → s2)
                 ('111 → s2)]))
