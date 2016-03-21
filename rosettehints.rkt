#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "rosettehintmethods.rkt")

(define (correct? S) #f)
(define wss (word-search-space (list 0 1) 4))

;; counterexample

(printf "Counterexample: ")

(define counterexample-generator (exists-word wss T counterexample-pred))
(define correct-behavior (λ () (printf "Correct.\n\n")))
(define ce-behavior (λ (w) (printf "The word ~a is a counterexample.\n\n" (word-value w))))
(define ce-backstop (λ () (printf "No counterexample could be found.\n\n")))

(define counterexample-hint (hint (cons correct? correct-behavior)
                                  (list (cons (timeout-hint counterexample-generator) ce-behavior))
                                  ce-backstop))
(counterexample-hint S)

; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(printf "Prefix: ")

(define prefix-generator (exists-word-forall-words wss T2 bad-prefix-pred))
(define prefix-behavior (λ (w) (printf "The prefix ~a followed by any word up to length ~a will not have the desired behavior.\n\n"
                                       (word-value w) 4)))
(define prefix-backstop (λ () (printf "No prefix of length ~a or less could be found.\n\n" 4)))

(define prefix-hint (hint (cons correct? correct-behavior)
                          (list (cons prefix-generator prefix-behavior))
                          prefix-backstop))

(prefix-hint S2)

;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(printf "Split state hint: ")

(define split-state-generator (exists-word-exists-word wss T3 split-state-pred))
(define split-state-behavior (λ (wlist) (printf "Words that arrive in the same state as ~a have different behaviors on the true solution.\n\n"
                                                (word-value (car wlist)))))
(define split-state-backstop (λ () (printf "No split state was found when checking words up to ~a in length.\n\n" 4)))

(define split-state-hint (hint (cons correct? correct-behavior)
                               (list (cons split-state-generator split-state-behavior))
                               split-state-backstop))

(split-state-hint S3)
