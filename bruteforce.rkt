#lang racket

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

(define (extend-word-by-one alphabet prevlist)
  (let ([lists (for/list ([i alphabet])
    (map (lambda (l) (append (list i) l)) prevlist))])
    (foldl append '() lists)))

(define (allwords alphabet)
  (letrec ([f (lambda (currentlist workinglist alphabet)
                (if (empty? workinglist)
                    (let ([newlist (extend-word-by-one alphabet currentlist)])
                          (stream-cons (car newlist) (f newlist (cdr newlist) alphabet)))
                    (stream-cons (car workinglist) (f currentlist (cdr workinglist) alphabet))))])
    (stream-cons '() (f (list null) null alphabet))))

;;;; counterexample ;;;;;

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

(define sigma2 (list (list 0 0) (list 0 1) (list 1 0) (list 1 1)))
(define binalpha (list 0 1))

;; helper function: w is accepted on m1 and rejected on m2 or vice versa.
(define (same-outcome? m1 m2 w)
  (eq? (m1 w) (m2 w)))

(define (find-counterexample M1 M2 alphabet k)
  (let ([s (allwords alphabet)])
    (letrec ([f (lambda (M1 M2 i k)
                  (cond [(not (same-outcome? M1 M2 (stream-ref s i))) (stream-ref s i)]
                        [(eq? i k) "no counterexample"]
                        [else (f M1 M2 (+ i 1) k)]))])
      (f M1 M2 0 k))))

(printf "Counterexample hint:\n")
(printf "The word ~a is a counterexample.\n\n" (find-counterexample S T (list 0 1) 20))

;;;;;; prefix hint ;;;;;;
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

(define (find-failing-prefix M1 M2 alphabet k)
  (let ([s (allwords alphabet)])
    (letrec ([f (lambda (M1 M2 i j k)
                  (cond [(not (same-outcome? M1 M2 (append (stream-ref s i) (stream-ref s j))))
                         (f M1 M2 i (+ j 1) k)] ;; p works with this suffix, keep checking suffixes
                        [(eq? i k) "no such prefix"] ;; finished searching
                        [(eq? j k) (stream-ref s i)] ;; found a prefix
                        [else (f M1 M2 (+ i 1) 0 k)]))]) ;; this prefix didn't work, check next prefix
      (f M1 M2 0 0 k))))
                                   



(printf "Prefix hint:\n")
;(printf "The prefix ~a st. p followed by any word up to length k will not have the desired behavior.\n\n" (find-failing-prefix S2 T2 binalpha 15)
(printf "Line above would find solution, but ran for >5min.\n\n")
;;;;;;; split state hint ;;;;;;;
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

(define (same-final-state M w wprime)
  (eq? (M w) (M wprime)))
(define (diff-outcomes M w wprime)
  (not (eq? (M w) (M wprime))))


(define (find-same-state-pair M1 M2 alphabet k)
  (let ([s (allwords alphabet)])
    (letrec ([f (lambda (M1 M2 i j k)
                  (cond [(and (same-final-state M1 (stream-ref s i) (stream-ref s j))
                              (diff-outcomes M2 (stream-ref s i) (stream-ref s j))) (list (M1 (stream-ref s i)) (stream-ref s i) (stream-ref s j))]
                        [(eq? i k) "no split state"]
                        [(eq? j k) (f M1 M2 (+ i 1) 0 k)]
                        [else (f M1 M2 i (+ j 1) k)]))])
      (f M1 M2 0 0 k))))

(define split-state-hint (find-same-state-pair S3 T3 binalpha 10))
(printf "\nSplit state hint:")
(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (car split-state-hint))
                                                                                
                  

    
