#lang s-exp rosette

(provide (struct-out word) automaton automaton2 automaton3 symbolic-word symbolic-word* same-outcome? counterexample-pred
         bad-prefix-pred split-state-pred)

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
       (let ([trace '()])
       (cond
         [(empty? stream) name]
         [else
          (case (first stream)
            [(label) (target (rest stream))] ...
            [else false])])))]
     ))

(define-syntax automaton2
  (syntax-rules (:)
    [(_ init-state
        (state : response ...)
        ...)
     (letrec ([state (process-state2 response ...)]
              ...)
              init-state)]))

(struct word (value))

(define-syntax process-state3
  (syntax-rules (→)
    [(_ name  (label → target) ...)
     (lambda (stream trace)
      ; (let ([trace '()])
       (cond
         [(empty? stream) (append trace (list name))]
         [else
          (case (first stream)
            [(label) (target (rest stream) (append trace (list name)))] ...
            [else false])]))]
     ))

(define-syntax automaton3
  (syntax-rules (:)
    [(_ init-state
        (state : response ...)
        ...)
     (letrec ([state (process-state3 response ...)]
              ...)
              init-state)]))

;; from https://github.com/emina/rosette/blob/master/sdsl/fsm/query.rkt
; Returns a symbolic word of length k, drawn from the given alphabet.
(define (symbolic-word k alphabet)
  (for/list ([i k])
    (define-symbolic* idx number?)
    (list-ref alphabet idx)))

; Returns a symbolic word of length up to k, drawn from the given alphabet.
(define (symbolic-word* k alphabet)
  (define-symbolic* n number?)
  (take (symbolic-word k alphabet) n))

;; helper function: w is accepted on m1 and rejected on m2 or vice versa.
(define (same-outcome? m1 m2 w)
  (eq? (m1 w) (m2 w)))

(define (counterexample-pred M1 M2 word)
  (not (eq? (M1 word) (M2 word))))

(define (bad-prefix-pred M1 M2 prefix word)
  (not (same-outcome? M1 M2 (append prefix word))))

(define (split-state-pred M1 M2 word wordprime)
  (and (eq? (M2 word) (M2 wordprime)) (not (eq? (M1 word) (M1 wordprime)))))
