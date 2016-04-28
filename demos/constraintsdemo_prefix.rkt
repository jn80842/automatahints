#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")
(require "exampledfas.rkt")

;; correctness checker
(define (correct? M) #f)

;; define word search space
(define wss (word-search-space (alphabet T2) 6))

;; predicate
;; automata -> automata -> word -> wordprime -> boolean
(define (bad-prefix? M1 M2 prefix w)
  (not (same-outcome? M1 M2 (append prefix w))))

;; new generator type: exists some word st for all words, predicate is true
(define prefix-generator
  (exists-word-forall-words wss T2 bad-prefix?))

(define correct-behavior (λ () (printf "Correct.\n\n")))

(define prefix-behavior
  (λ (w) (printf "The prefix ~a followed by any word up to length ~a will not have the desired behavior.\n\n"
                                       (word-value w) 6)))
(define prefix-backstop
  (λ () (printf "No failing prefix of length ~a or less could be found.\n\n." 6)))

(define prefix-hint (hint (cons correct? correct-behavior)
                          (list (cons prefix-generator prefix-behavior))
                          prefix-backstop))

(prefix-hint S2)

