#lang racket

(require racket/generator)
(require "automata.rkt")

(provide allwords bounded-words print-stream find-counterexample find-failing-prefix find-split-state find-split-state-gen find-counterexample-gen find-failing-prefix-gen)
(define sigma2 (list "00" "01" "10" "11"))
(define firstlist (list (list "00") (list "01" ) (list "10") (list "11")))

(define (extend-word-by-one alphabet prevlist)
  (let ([lists (for/list ([i alphabet])
    (map (lambda (l) (cons i l)) prevlist))])
    (append* lists)))

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

(define (wordgenerator alphabet)
  (generator ()
             (begin
               (yield '())
               (let loop ([workinglst (map (lambda (x) (list x)) alphabet)]
                          [originallst (map (lambda (x) (list x)) alphabet)])
                 (if (empty? workinglst)
                     (loop (extend-word-by-one alphabet originallst) (extend-word-by-one alphabet originallst))
                     (begin
                       (yield (car workinglst))
                       (loop (cdr workinglst) originallst)))))))

(define (print-stream s k)
  (for/list ([i k])
    (printf "~a\n" (stream-ref s i))))

(define (bfsame-outcome? M1 M2 w)
  (not (eq? (M1 w) (M2 w))))

(define (find-counterexample M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)])
  (let ([s (stream-filter (lambda (w) (not (eq? (M1 w) (M2 w)))) (bounded-words alphabet limit))])
    (if (stream-empty? s) "no counterexample" (stream-first s)))))

(define (words-up-to-k alphabet-length k)
  (for/sum ([i (in-range (+ k 1))]) (expt alphabet-length i)))

(define (find-counterexample-gen M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) "no counterexample"]
                          [(not (same-outcome? M1 M2 w)) w]
                          [else (f (add1 i))])))])
      (f 1))))


(define (find-failing-prefix M1 M2 alphabet k)
  (let ([s (stream-filter (lambda (p) (stream-andmap
                                            (lambda (w) (not (bfsame-outcome? M1 M2 (append p w)))) (bounded-words alphabet k)))
                               (bounded-words alphabet k))])
    (if (stream-empty? s) "no such prefix" (stream-first s))))

(define (find-failing-prefix-gen M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                (let ([p (gen)])
                  (cond [(eq? i limit) "no such prefix"]
                        [(failing-prefix M1 M2 alphabet k p) p]
                        [else (f (add1 i))])))])
      (f 1))))

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

(define (find-split-state-gen M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i j w gen2)
                  (let ([wprime (gen2)])
                    (cond [(eq? i limit) "no split state"]
                          [(eq? j limit) (f (add1 i) 1 (gen) (wordgenerator alphabet))]
                          [(and (same-final-state M1 w wprime ) (diff-outcomes M2 w wprime)) (list (M1 w) w wprime)]
                          [else (f i (add1 j) w gen2)])))])
      (f 1 1 (gen) (wordgenerator alphabet)))))