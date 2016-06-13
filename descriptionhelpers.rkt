#lang racket

(require "automata.rkt")

(provide (all-defined-out))

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


;;; predicates
(define (greater-than-k k symbol w)
  (if (empty? w)
      #f
      (< k (count (λ (s) (eq? symbol s)) (word-value w)))))

(define (greater-eq-k k symbol w)
  (if (empty? w)
      #f
      (<= k (count (λ (s) (eq? symbol s)) (word-value w)))))

(define (less-eq-k k symbol w)
  (if (empty? w)
      #f
      (>= k (count (λ (s) (eq? symbol s)) (word-value w)))))

(define (exactly-k k symbol w)
  (if (empty? w)
      #f
      (eq? k (count (λ (s) (eq? symbol s)) (word-value w)))))

(define count-descriptions (list (cons "Words with at least ~a occurrences of '~a'"
                                 greater-eq-k)
                           (cons "Words with at most ~a occurrences of '~a'"
                                 less-eq-k)
                           (cons "Words with exactly ~a occurrences of '~a'"
                                 exactly-k)))

(define (is-prefix? prefix w)
  (if (or (empty? w) (empty? prefix) (empty? (word-value prefix)))
      #f
      (eq? (length (word-value prefix)) (length (take-common-prefix (word-value prefix) (word-value w))))))

(define (is-suffix? suffix w)
  (eq? (length (word-value suffix)) (length (take-common-prefix (reverse (word-value suffix)) (reverse (word-value w))))))

(define (is-substring? substring w)
   (for/first ([i (range 0 (length (word-value w)))]
               #:when (eq? (length (word-value substring))
                           (length (take-common-prefix (word-value substring) (drop (word-value w) i)))))
     #t))

(define substring-descriptions (list (cons "Words with the prefix '~a'" is-prefix?)
                                     (cons "Words with the suffix '~a'" is-suffix?)
                                     (cons "Words with the substring '~a'" is-substring?)))


                                             



