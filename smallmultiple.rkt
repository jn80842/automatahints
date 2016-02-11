#lang racket

(require racket/generator)
(require "automata.rkt")

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

(define (mutate-del word)
  (in-generator
             (let loop ([lprime '()]
                        [ele (car word)]
                        [l (cdr word)])
               (if (empty? l)
                   (begin
                     (yield lprime)
                     '())
                   (begin
                     (yield (append lprime l))
                     (loop (append lprime (list ele)) (car l) (cdr l)))))))

(define (mutate-add word alphabet)
  (in-generator
             (let loop ([lprime '()]
                        [l word])
               (if (empty? l)
                   (begin
                     (for ([a alphabet])
                       (yield (append lprime (list a))))
                     '())
                   (begin
                     (for ([a alphabet])
                       (yield (append lprime (list a) l)))
                     (loop (append lprime (list (car l))) (cdr l)))))))

(define (mutate-sub word alphabet)
  (in-generator
             (let loop ([lprime '()]
                        [ele (car word)]
                        [l (cdr word)])
               (if (empty? l)
                   (begin
                     (for ([a alphabet] #:unless (eq? a ele))
                       (yield (append lprime (list a))))
                     '())
                   (begin
                     (for ([a alphabet] #:unless (eq? a ele))
                       (yield (append lprime (list a) l)))
                     (loop (append lprime (list ele)) (car l) (cdr l)))))))




(define (mutate w alphabet)
  (generator ()
             (begin
               (for ([wprime (mutate-del w)])
                 (yield wprime))
               (for ([wprime (mutate-add w alphabet)])
                 (yield wprime))
               (for ([wprime (mutate-sub w alphabet)])
                 (yield wprime))
               null)))

;; this code copied from bruteforcemethods for convenience

(define (extend-word-by-one alphabet prevlist)
  (let ([lists (for/list ([i alphabet])
    (map (lambda (l) (cons i l)) prevlist))])
    (append* lists)))

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

(define (words-up-to-k alphabet-length k)
  (for/sum ([i (in-range (+ k 1))]) (expt alphabet-length i)))

(define (one-away M1 M2 w alphabet)
  (let ([m (mutate w alphabet)])
    (letrec ([f (lambda ()
                  (let ([wprime (m)])
                    (cond [(empty? wprime) null]
                          [(same-outcome? M1 M2 wprime) (word wprime)]
                          [(f)])))])
      (f))))
                        
(define (exists-small-multiple M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) "no such word found"]
                          [(not (same-outcome? M1 M2 w)) (let ([wprime (one-away M1 M2 w alphabet)])
                                                           (if (empty? wprime) (f (add1 i)) (list (word w) wprime)))]
                          [else (f (add1 i))])))])
      (f 1))))
               

  
  