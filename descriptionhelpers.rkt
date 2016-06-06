#lang racket

(require "automata.rkt")

(provide equal-word? greater-word? lesser-word?)

;; null words aren't equal to anything
(define (equal-word? w wprime)
  (and (not (empty? w))
       (and (not (empty? wprime))
            (and (eq? (length (word-value w)) (length (word-value wprime)))
                 (andmap eq? (word-value w) (word-value wprime))))))

;; assume that symbols can be compared with > and <
(define (greater-word? w wprime)
  (if (or (empty? w) (empty? wprime))
      #f
      (let ([wval (word-value w)]
            [wpval (word-value wprime)])
        (cond [(> (length wval) (length wpval)) #t]
              [(< (length wval) (length wpval)) #f]
              [else (for/first ([s1 wval]
                                [s2 wpval]
                                #:when (not (eq? s1 s2)))
                      (> s1 s2))]))))

(define (lesser-word? w wprime)
  (if (or (empty? w) (empty? wprime))
      #f
      (let ([wval (word-value w)]
            [wpval (word-value wprime)])
        (cond [(< (length wval) (length wpval)) #t]
              [(> (length wval) (length wpval)) #f]
              [else (for/first ([s1 wval]
                                [s2 wpval]
                                #:when (not (eq? s1 s2)))
                      (< s1 s2))]))))