#lang s-exp rosette

(require
  rosette/query/debug rosette/lib/tools/render
  rosette/lib/meta/meta)

(require "automata.rkt")
(require "rosettehintmethods.rkt")
(require "bruteforcemethods.rkt")

(define smallalpha (list 0 1 2))
(define medalpha (list 0 1 2 3 4 5))
(define largealpha (list 0 1 2 3 4 5 6 7 8 9))

;; small states 3
;; medium states 6
;; large states 10

;; sink states/no sink states

;; type of variance between S and T

(define medmedS
  (automaton s0
             [s0 : (0 → s2)
                 (1 → s1)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]
             [s1 : (0 → s1)
                 (1 → s1)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]
             [s2 : accept (0 → s1)
                 (1 → s3)
                 (2 → s2)
                 (3 → s3)
                 (4 → s2)]
             [s3 : (0 → s4)
                 (1 → s4)
                 (2 → s3)
                 (3 → s4)
                 (4 → s2)]
             [s4 : (0 → s3)
                 (1 → s4)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]))

(define medmedT
  (automaton s0
             [s0 : (0 → s2)
                 (1 → s1)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]
             [s1 : (0 → s1)
                 (1 → s1)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]
             [s2 : accept (0 → s1)
                 (1 → s3)
                 (2 → s2)
                 (3 → s3)
                 (4 → s2)]
             [s3 : (0 → s4)
                 (1 → s4)
                 (2 → s2)
                 (3 → s4)
                 (4 → s2)]
             [s4 : (0 → s3)
                 (1 → s1)
                 (2 → s4)
                 (3 → s1)
                 (4 → s4)]))

(printf "Rosette counterexample hint:\n")
(printf "The word ~a is a counterexample.\n\n" (solve-automaton-ce medmedS medmedT medalpha 3))

(printf "Enumerative counterexample hint:\n")
(printf "The word ~a is a counterexample.\n\n" (find-counterexample medmedS medmedT medalpha 3))

(printf "Rosette split state hint:\n")
;(printf "Split state ~a.\n\n" (solve-split-state medmedS medmedT medalpha 10))

(printf "Enumerative split state hint:\n")
;(printf "Split state ~a.\n\n" (find-split-state medmedS medmedT medalpha 20))