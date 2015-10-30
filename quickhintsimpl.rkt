#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

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


(define S
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s0)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define T
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))
             
            
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

(printf "Counterexample hint:\n")
(define (solve-automaton-ce m1 m2 k)
  (define w (word* k '(0 1)))
  (evaluate w (solve (assert (not (same-outcome? m1 m2 w))))))

(printf "The word ~a is a counterexample.\n\n" (solve-automaton-ce S T 3))

; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(define S2
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define T2
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s2)
                 (1 → s2)]))

(printf "Prefix hint:\n")
(define (prefixer w)
         (append (list (??) (??) (??)) w))

(define (prefix-check m1 m2 prefixer w)
  (assert (not (same-outcome? m1 m2 (prefixer w)))))
(define ww (word* 3 '(0 1)))
(define binding
  (synthesize #:forall (list ww)
              #:guarantee (prefix-check S2 T2 prefixer ww)))
(printf "This synthesized function contains a prefix p st. p followed by any word up to length k will not have the desired behavior.\n\n")
(print-forms binding)

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


(define S3
  (automaton2 s0
              [s0 : "s0" (0 → s1)
                  (1 → s3)]
              [s1 : "s1" (0 → s3)
                  (1 → s2)]
              [s2 : "s2" (0 → s2)
                  (1 → s2)]
              [s3 : "s3" (0 → s3)
                  (1 → s3)]))
(define T3
  (automaton s0
             [s0 : accept (0 → s1)
                 (1 → s0)]
             [s1 : accept (0 → s1)
                 (1 → s2)]
             [s2 : (0 → s2)
                 (1 → s2)]))

(printf "\nSplit state hint:\n")
(define (solve-split-state m1 m2 k)
  (define w (word* k '(0 1)))
  (define wprime (word* k '(0 1)))
  (m1 (evaluate w (solve (begin (assert (eq? (m1 w) (m1 wprime)))
                (assert (not (eq? (m2 w) (m2 wprime)))))))))

(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (solve-split-state S3 T 3))
