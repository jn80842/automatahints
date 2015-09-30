#lang racket
(require unstable/automata/machine)
(require unstable/automata/dfa)

; pipeline:
; generator -> checker -> (fitness scorer ->) presentation
; interface between the layers not yet determined
; original inputs that all layers have access to: student solution, correct solution, problem statement
; scorer would take hints that pass checker and return highest scoring to presentation layer

;;;; sample DFA pair ;;;;;

; accepts strings that contain exactly 2 0s
(define true-solution
  (dfa s0 (s2)
       [s0 ([0 s1]
            [1 s0])]
       [s1 ([0 s2]
            [1 s1])]
       [s2 ([0 s3]
            [1 s2])]
       [s3 ([0 s3]
            [1 s3])]))

; accepts strings that contain no 0s after seeing two consecutive 0s
(define student-solution
  (dfa s0 (s2)
       [s0 ([0 s1]
            [1 s0])]
       [s1 ([0 s2]
            [1 s0])]
       [s2 ([0 s3]
            [1 s2])]
       [s3 ([0 s3]
            [1 s3])]))
     

;;;; counterexample ;;;;;

; generator: stream that returns strings in Sigma*

; adapted from http://rosettacode.org/wiki/Find_palindromic_numbers_in_both_binary_and_ternary_bases#Racket
(define (number->list/base n b)
  (define (inr acc n)
    (if (zero? n) acc
        (let-values (((q r) (quotient/remainder n b)))
          (inr (cons r acc) q))))
  (if (zero? n) '(0) (inr null n)))

(define (number->paddedlist/base n b)
  (if (eq? n 1) '()
  (let ([l (inexact->exact (floor (/ (log n) (log b))))])
  (let ([bl (number->list/base (- n (expt b l)) b)])
    (append (build-list (- l (length bl)) (lambda (x) (* x 0))) bl)
  ))))
        
(define (sigma-nats sigma)
  (let ([ht (make-hash)])
    (for/list ([i (in-naturals 0)] [val sigma]) (hash-set! ht i val))
    (let ([htmap (lambda (x) (hash-ref ht x))])
    (letrec ([f (lambda (x) (cons (map htmap (number->paddedlist/base x (length sigma))) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))))

; checker: does string has different behavior on true and student solution?
(define (counterexample-checker S T word)
  (not (eq? (machine-accepts? S word) (machine-accepts? T word))))

; combined

(define (stream-hint-tester stream checker S T)
  (letrec ([f (lambda (stream)
                (let ([pr (stream)])
                  (if (checker S T (car pr))
                     (string-join (map number->string (car pr)) "")
                     (f (cdr pr)))))])
    (f stream)))

; presentation
(define (counterexample-present word)
  (format "Consider the string '~a'" word))


(counterexample-present (stream-hint-tester (sigma-nats '(0 1)) counterexample-checker student-solution true-solution))


;;;; misclassified strings by proper prefix
;;;; proper prefix for *some* misclassified strings that is not a prefix of *any* correctly classified strings

; generator

; checker 

; presentation
(define (misprefix-present prefix)
  (format "Consider strings with the prefix '~a'" prefix))


;;;; string arrives in wrong state ;;;;;

; generator: stream of triples (word, state reached in student DFA, state reached in true DFA)

; checker: is accept status of states different?

; presentation: text
(define (wrongstate-present S-state-label S-state-accepts?)
  (format "Strings that should be ~a can arrive in state ~a" (if S-state-accepts? "rejected" "accepted") S-state-label))