#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")
(require "exampledfas.rkt")

;;;; counterexample ;;;;;
(printf "Counterexample hint:\n")

;; correctness checker
(define (correct? S) #f)

; predicate
; automaton -> automaton -> word -> boolean
(define (counterexample? S T w)
  (not (eq? (S w) (T w))))

;; struct that holds all the information we need to generate words
(define wss (word-search-space (alphabet T) 4))

;; generator for counterexamples
(define counterexample-generator
  (exists-word wss T counterexample?))

;; wrap generator in timeout
(define timed-counterexample-generator (timeout-hint counterexample-generator 5000))

;; three possible outcomes:
;; student submission is correct,
;; submission is incorrect and produces a hint,
;; submission is incorrect and a hint cannot be found.
(define correct-behavior (λ () (printf "Correct.\n\n")))
(define ce-behavior (λ (w) (printf "The word ~a is a counterexample.\n\n" (word-value w))))
(define ce-backstop (λ () (printf "No counterexample could be found.\n\n")))

;; a hint consists of pairs of functions and behaviors
(define counterexample-hint
  (hint (cons correct? correct-behavior)
                                  (list (cons timed-counterexample-generator ce-behavior))
                                  ce-backstop))

;; the result is a function that takes the student submission as an argument
(counterexample-hint S)


