#lang s-exp rosette

(provide automaton automaton2 word word* same-outcome?)


; adapted from Automata via Macros (Krishnamurthi)
; and Emina's automaton in Rosette
(define-syntax process-state
  (syntax-rules (accept →)
    [(_ accept (label → target) ...)
     (lambda (stream)
       (cond
         [(empty? stream) true]
         [else
          (case (first stream)
            [(label) (target (rest stream))] ...
            [else false])]))]
    [(_(label → target) ...)
     (lambda (stream)
       (cond
         [(empty? stream) false]
         [else
          (case (first stream)
            [(label) (target (rest stream))]
            ...
            [else false])]))]
     ))

(define-syntax automaton
  (syntax-rules (:)
    [(_ init-state
        (state : response ...)
        ...)
     (letrec ([state (process-state response ...)]
              ...)
              init-state)]))

;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(define-syntax process-state2
  (syntax-rules (→)
    [(_ name  (label → target) ...)
     (lambda (stream)
       (cond
         [(empty? stream) name]
         [else
          (case (first stream)
            [(label) (target (rest stream))] ...
            [else false])]))]
     ))

(define-syntax automaton2
  (syntax-rules (:)
    [(_ init-state
        (state : response ...)
        ...)
     (letrec ([state (process-state2 response ...)]
              ...)
              init-state)]))

;; from https://github.com/emina/rosette/blob/master/sdsl/fsm/query.rkt
; Returns a symbolic word of length k, drawn from the given alphabet.
(define (word k alphabet)
  (for/list ([i k])
    (define-symbolic* idx number?)
    (list-ref alphabet idx)))

; Returns a symbolic word of length up to k, drawn from the given alphabet.
(define (word* k alphabet)
  (define-symbolic* n number?)
  (take (word k alphabet) n))

;; helper function: w is accepted on m1 and rejected on m2 or vice versa.
(define (same-outcome? m1 m2 w)
  (eq? (m1 w) (m2 w)))
