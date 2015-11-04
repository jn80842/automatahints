#lang racket

(require "automata.rkt")

(provide allwords bounded-words print-stream find-counterexample find-failing-prefix find-split-state)

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

(define (bfsame-outcome? M1 M2 w)
  (not (eq? (M1 w) (M2 w))))

(define (find-counterexample M1 M2 alphabet k)
  (let ([s (stream-filter (lambda (w) (not (eq? (M1 w) (M2 w)))) (bounded-words alphabet k))])
    (if (stream-empty? s) "no counterexample" (stream-first s))))

(define (find-failing-prefix M1 M2 alphabet k)
  (let ([s (stream-filter (lambda (p) (stream-andmap
                                            (lambda (w) (not (bfsame-outcome? M1 M2 (append p w)))) (bounded-words alphabet k)))
                               (bounded-words alphabet k))])
    (if (stream-empty? s) "no such prefix" (stream-first s))))

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