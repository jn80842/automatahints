#lang racket
(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")
(require "description.rkt")
(require "descriptionhelpers.rkt")

;; find 2 equivalence classes in true solution
;; that are subsets of the same equivalence class in student solution

(define S3-s0
  (automaton s0
             [s0 : (accept : #t)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S3-s1
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S3-s2
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

(define S3-s3
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #t)
                 (0 → s3)
                 (1 → s3)]))


(define T3-s0
  (automaton s0
             [s0 : (accept : #t)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s1)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]))

(define T3-s1
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #t)
                 (0 → s1)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]))

(define T3-s2
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

(define student-eq-classes (list (dfa-lang S3-s0)
                                 (dfa-lang S3-s1)
                                 (dfa-lang S3-s2)
                                 (dfa-lang S3-s3)))
(define true-eq-classes (list (dfa-lang T3-s0)
                              (dfa-lang T3-s1)
                              (dfa-lang T3-s2)))

(define (subset-eq classes1 classes2)
  (for*/first ([eqclass1a classes1]
               [eqclass1b (filter (λ (e) (not (eq? e eqclass1a))) classes1)]
               [eqclass2 classes2]
               #:when (and (subset-lang? eqclass1a eqclass2) (subset-lang? eqclass1b eqclass2)))
    (list eqclass1a eqclass1b eqclass2)))

;;; also try to synthesize suffix for which one word will lead to word being accepted
;;; one being rejected on true solution

;;; what to describe?
;;; two equivalence classes on true solution, each has subset that arrives in same state on student sol
;;; describe union of those 2 subsets
