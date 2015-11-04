#lang racket

(require "automata.rkt")

(provide allwords bounded-words find-counterexample find-failing-prefix find-split-state)

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

(define (bounded-words alphabet k)
  (letrec ([f (lambda (currentlist workinglist alphabet i k)
                (cond [(> i k) empty-stream]
                      [(and (empty? workinglist) (< i k))
                       (let ([newlist (extend-word-by-one alphabet currentlist)])
                         (stream-cons (car newlist) (f newlist (cdr newlist) alphabet (+ i 1) k)))]
                      [(empty? workinglist) empty-stream]
                      [else (stream-cons (car workinglist) (f currentlist (cdr workinglist) alphabet i k))]))])
    (stream-cons '() (f (list null) null alphabet 0 k))))

(define (print-stream s k)
  (for/list ([i k])
    (printf "~a\n" (stream-ref s i))))

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
  (let ([s (stream-filter (lambda (w) (not (same-outcome? M1 M2 w))) (bounded-words alphabet k))])
    (if (stream-empty? s) "no counterexample" (stream-first s))))

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
  (let ([s (stream-filter (lambda (p) (stream-andmap
                                            (lambda (w) (not (same-outcome? M1 M2 (append p w)))) (bounded-words alphabet k)))
                               (bounded-words alphabet k))])
    (if (stream-empty? s) "no such prefix" (stream-first s))))

(define allwords3 (list '() '(1) '(0) '(0 0) '(0 1) '(1 0) '(1 1) '(0 0 0) '(0 0 1) '(0 1 0) '(0 1 1) '(1 0 0) '(1 0 1) '(1 1 0) '(1 1 1)))

;; fast prefix solution
(filter (lambda (p) (andmap (lambda (w) (not (same-outcome? S2 T2 (append p w)))) allwords3)) allwords3)

(printf "Prefix hint:\n")
(printf "The prefix ~a st. p followed by any word up to length k will not have the desired behavior.\n\n" (find-failing-prefix S2 T2 binalpha 3))

;;;;;;; split state hint ;;;;;;;
;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

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


(define (find-split-state M1 M2 alphabet k)
  (let ([s (allwords alphabet)])
    (letrec ([f (lambda (M1 M2 i j k)
                  (cond [(and (same-final-state M1 (stream-ref s i) (stream-ref s j))
                              (diff-outcomes M2 (stream-ref s i) (stream-ref s j))) (list (M1 (stream-ref s i)) (stream-ref s i) (stream-ref s j))]
                        [(eq? i k) "no split state"]
                        [(eq? j k) (f M1 M2 (+ i 1) 0 k)]
                        [else (f M1 M2 i (+ j 1) k)]))])
      (f M1 M2 0 0 k))))

(define split-state-hint (find-split-state S3 T3 binalpha 10))
(printf "\nSplit state hint:")
(printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (car split-state-hint))
                                                                                
                  

    