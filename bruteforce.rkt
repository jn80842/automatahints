#lang racket

(require racket/engine)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")

;; fake correctness checker, since all our examples are wrong
(define (correct? S) #f)

;;;; counterexample ;;;;;
(printf "Counterexample hint:\n")

(define word-gen (words (alphabet T))) ;; unbounded word generator
(define wss (word-search-space (alphabet T) 4))

(define counterexample-generator
  (exists-word wss T counterexample-pred))

(define correct-behavior (λ () (printf "Correct.\n\n")))
(define ce-behavior (λ (w) (printf "The word ~a is a counterexample.\n\n" (word-value w))))
(define ce-backstop (λ () (printf "No counterexample could be found.\n\n")))

(define counterexample-hint (hint (cons correct? correct-behavior)
                                  (list (cons (timeout-hint counterexample-generator) ce-behavior))
                                  ce-backstop))

(counterexample-hint S)

;;;;;; prefix hint ;;;;;;
; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(printf "Prefix hint:\n")

(define prefix-generator
  (exists-word-forall-words wss T2 bad-prefix-pred))

(define prefix-behavior (λ (w) (printf "The prefix ~a followed by any word up to length ~a will not have the desired behavior.\n\n"
                                       (word-value w) 4)))
(define prefix-backstop (λ () (printf "No prefix of length ~a or less could be found.\n\n." 4)))

(define prefix-hint (hint (cons correct? correct-behavior)
                          (list (cons prefix-generator prefix-behavior))
                          prefix-backstop))

(prefix-hint S2)
;;;;;;; split state hint ;;;;;;;
;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(printf "\nSplit state hint:")

(define split-state-generator (exists-word-exists-word wss T3 split-state-pred))
; this is a hack -- want to give the state name & not a word
; but need to re-architect so this method has access to the student automaton
(define split-state-behavior (λ (wlist) (printf "Words that arrive in the same state as ~a have different behaviors on the true solution.\n\n"
                                                (word-value (car wlist)))))
(define split-state-backstop (λ () (printf "No split state was found when checking words up to ~a in length.\n\n" 10)))

(define split-state-hint (hint (cons correct? correct-behavior)
                               (list (cons split-state-generator split-state-behavior))
                               split-state-backstop))

(split-state-hint S3)
