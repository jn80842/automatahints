#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")

;;;; Instructor writes a problem ;;;;

(printf "Write a DFA that accepts all strings containing exactly 2 0's.\n\n")

;;;; Instructor writes the correct answer ;;;;

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

;;;; Instructor wants to write a counterexample hint. The information returned to the student
;;;; is a word with different behavior on the true and supplied solution.

;;;; Instructor writes a boolean function (automaton -> automaton -> word -> bool) which is true
;;;; if the word is a valid counterexample

(define (ce-pred S T word)
  (not (eq? (S word) (T word))))

;;;; Supply true solution and predicate to exists-word-f to get a function that takes the student
;;;; solution as its only argument.
;;;; Note that this function embeds the choice of search strategy (enumerative).
;;;; (automaton -> (a -> a -> w -> bool) -> (a -> word)

(define ce-func (exists-word-f T ce-pred))

;;;; Let's assume we have a correctness checker

(define (correct-check S)
  #f)

;;;; This function is wrapped in a presentation layer

(define (ce-view hint-func)
  (lambda (S)
    (if (correct-check S)
        (printf "Correct.\n\n")
        (let ([hint (hint-func S)])
          (if (empty? hint)
              (printf "No counterexample could be found.\n\n")
              (printf "The word ~a is a counterexample.\n\n" (word-value hint)))))))

(define ce-hint (ce-view ce-func))

;;;; The student writes their answer to the problem

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

(printf "(Student submits incorrect solution.)\n\n")

;;;; Upon submission, the hint is displayed

(ce-hint S)






