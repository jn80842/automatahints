#lang s-exp rosette

(require racket/engine)

(provide (struct-out word-search-space)
         (struct-out word)
         (struct-out fsm)
         automaton automaton2 automaton3
         symbolic-word symbolic-word*
         same-outcome? counterexample-pred bad-prefix-pred split-state-pred
         alphabet states bounded-word-length timeout-hint hint)

(struct word (value))

(struct word-search-space (alphabet k))

(struct fsm (graph exec)
  #:property prop:procedure
  (struct-field-index exec))

; adapted from Automata via Macros (Krishnamurthi)
; and Emina's automaton examples in Rosette
(define-syntax automaton
  (syntax-rules (: → accept)  
    [(_ init-state
        (state : (accept : acceptstate)
               (label → target) ...) ...)
     (letrec ([state
               (λ (stream)
                 (cond
                   [(empty? stream) acceptstate]  
                   [else
                    (case (first stream)
                      [(label) (target (rest stream))] ...
                      [else false])]))]
              ...) 
       (fsm '((state (label target) ...) ...) init-state))]

    ))

;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(define-syntax automaton2
  (syntax-rules (:)
    [(_ init-state
        (state : name (label → target) ... ) ...)
     (letrec ([state
               (λ (stream)
       (let ([trace '()])
       (cond
         [(empty? stream) name]
         [else
          (case (first stream)
            [(label) (target (rest stream))] ...
            [else false])])))]
              ...)
             (fsm '((state (label target) ... ) ...) init-state))]))

(define-syntax automaton3
  (syntax-rules (: →)
    [(_ init-state
        (state : name (label → target) ...)
        ...)
     (letrec ([state
               (λ (stream trace)
      ; (let ([trace '()])
       (cond
         [(empty? stream) (append trace (list name))]
         [else
          (case (first stream)
            [(label) (target (rest stream) (append trace (list name)))] ...
            [else false])]))]
              ...)
              (fsm '((state (label target) ... ) ... ) init-state))]))

(define (alphabet m)
  (remove-duplicates 
   (for/fold ([out '()]) ([ne (fsm-graph m)] #:unless (null? (cdr ne)))
     (append out (map car (cdr ne))))))

(define (states m)
  (remove-duplicates 
   (for/fold ([out '()]) ([ne (fsm-graph m)] #:unless (null? (cdr ne)))
     (append out (map cadr (cdr ne))))))

(define (bounded-word-length M1 M2)
  (* (length (states M1)) (length (states M2))))

(define (timeout-hint hint-generator [timeout 10000])
  (λ (S)
    (let ([hint-engine (engine (λ (_) (hint-generator S)))])
      (engine-run timeout hint-engine)
      (engine-result hint-engine))))

(define (hint correct-behavior hint-chain fallback)
  (λ (S)
    (if ((car correct-behavior) S)
        (cadr correct-behavior)
        (unless (for*/first ([hint-pair hint-chain]
                            #:when (not (empty? ((car hint-pair) S))))
                  ((cdr hint-pair) ((car hint-pair) S)))
          (fallback)))))

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

(define (bad-prefix-pred M1 M2 prefix w)
  (not (same-outcome? M1 M2 (append prefix w))))

(define (split-state-pred M1 M2 word wordprime)
  (and (eq? (M1 word) (M1 wordprime)) (not (eq? (M2 word) (M2 wordprime)))))
