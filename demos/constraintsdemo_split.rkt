#lang racket

(require "automata.rkt")
(require "bruteforcemethods.rkt")
(require "exampledfas.rkt")

; correctness checker
(define (correct? M) #f)

(define wss (word-search-space (alphabet T3) 10))

(printf "\nSplit state hint: \n")

;; automata -> automata -> word -> wordprime -> boolean
(define (split-state? M1 M2 word wordprime)
  (and (eq? (M1 word) (M1 wordprime)) (not (eq? (M2 word) (M2 wordprime)))))

(define split-state-generator (exists-word-exists-word wss T3 split-state-pred))

(define correct-behavior (λ () (printf "Correct.\n\n")))

(define split-state-behavior (λ (wlist) (printf "Words that arrive in the same state as the word ~a have different behaviors on the true solution.\n\n"
                                                (word-value (car wlist)))))
(define split-state-backstop (λ () (printf "No split state was found when checking words up to ~a in length.\n\n" 10)))

(define split-state-hint (hint (cons correct? correct-behavior)
                               (list (cons split-state-generator split-state-behavior))
                               split-state-backstop))

(split-state-hint S3)
