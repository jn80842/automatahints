#lang racket
(require unstable/automata/machine)
(require unstable/automata/dfa)

;;;;; helper functions ;;;;;
(define (good-outcome-word S T word)
  (eq? (machine-accepts? S word) (machine-accepts? T word)))
(define (bad-outcome-word S T word)
  (not (eq? (machine-accepts? S word) (machine-accepts? T word))))
;; add function 'consumed' / 'not-stuck'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            hints belong in NP                   ;;
;; generator provides hint & certificate           ;;
;; once proven, use certificate to measure fitness ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; validate counterexample ;;;;
;assumption that L(S) and L(T) not equal
(define (counterexample-checker word S T)
  (not (eq? (machine-accepts? S word) (machine-accepts? T word))))
; fitness: shorter word is better

;;;; minimal pairs ;;;;
(define (minimalpairs-checker S T )
  (and (eq? (length goodlist) (length badlist)) ; or, desired length of min pairs list
       (and (andmap (lambda (w) (good-outcome-word S T w) goodlist)
            (andmap (lambda (w) (bad-outcome-word S T w) badlist))))
; fitness: shorter edit distance between pairs is better

;;;; strings arrive in wrong state ;;;;
(define (wrongstate-checker S T)
  (and (andmap (lambda (w) (good-outcome-word S T w)) cert-list)
       (andmap (lambda (s) (eq? s (final-state S (car cert-list)))) cert-list)))
; fitness for this and next 2 hints: longer lists of cert words better
; such lists are likely either finite and fairly short or arbitrarily long however

;;;; split states ;;;;
(define (splitstate-checker S T goodlist badlist)
  (and (andmap (lambda (w) (good-outcome-word S T w)) goodlist)
       (and (andmap (lambda (w) (bad-outcome-word S T w)) badlist)
            (andmap (lambda (s) (eq? s (final-state S (car goodlist)))) (append goodlist badlist)))))

;;;; prefix of some misclassified strings ;;;;
; prove that all strings with this prefix are bad OR prove that all bad strings have this prefix: universally quantified
; prove that from this prefix many words don't arrive in good state
(define (misclassifiedprefix-checker S T)
  (andmap (lambda (w) (invalid-word S T (append prefix w))) cert-list))

;;;; synthesized descriptions of states ;;;;
; certificate is list of mosel descriptions
; show that for each state in S, there is one & only one mosel description that matches DFA but with that state as its only final state
; consider mosel -> English transformation as presentation layer, so no need to check

;;;; heat map ;;;;
; for all strings (under certain length), show proportion of their final states
; this seems inherently universally quantified: how to put this in NP?



