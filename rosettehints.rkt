#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "rosettehintmethods.rkt")

(provide same-outcome? solve-automaton-ce solve-split-state)

;(printf "The word ~a is a counterexample.\n\n" (solve-automaton-ce S T (list 0 1) 3))
(printf "Counterexample hint:\n")
(define ce (exists-word S T counterexample-pred))
(if (empty? ce)
    (printf "No counterexample of size ~a of less was found.\n\n" 3)
    (printf "The word ~a is a counterexample.\n\n" (word-value ce)))

; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(printf "Prefix hint:\n")
(define prefix (exists-word-forall-words S2 T2 bad-prefix-pred))
(if (empty? prefix)
    (printf "No prefix of length ~a or less was found.\n\n" 3)
    (printf "The prefix ~a st. p followed by any word up to length ~a will not have the desired behavior.\n\n" (word-value prefix) 3))
;(define (prefixer w)
;  (define p (list (??) (??) (??)))
;         (append p w))

;(define (prefix-check m1 m2 prefixer w)
;  (assert (not (same-outcome? m1 m2 (prefixer w)))))

;(define ww (symbolic-word* 3 '(0 1)))
;(define binding
;  (synthesize #:forall (list ww)
;              #:guarantee (prefix-check S2 T2 prefixer ww)))
;(printf "This synthesized function contains a prefix p st. p followed by any word up to length k will not have the desired behavior.\n\n")
;(print-forms binding)

;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T


(printf "\nSplit state hint:\n")

;(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (solve-split-state S3 T3 (list 0 1) 3))
(define split-state-words (exists-word-exists-word S3 T3 split-state-pred))

(if (empty? split-state-words)
    (printf "\nNo split state was found when checking words up to ~a in length.\n\n" 10)
    (printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (S3 (word-value (car split-state-words)))))