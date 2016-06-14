#lang racket

(require racket/generator)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "descriptionhelpers.rkt")
(require "languageoperators.rkt")
(require "predicatelanguage.rkt")

;; running examples
(define student-language (dfa-lang S3 "prefix 01"))
(define true-language (dfa-lang T3 "substring 01"))

;; compose two DFA-based languages to get a language of counterexamples
(define counterexample-lang (union-lang (set-diff-lang student-language true-language) (set-diff-lang true-language student-language)))


;; if we pick k=2, symbol=0, we can check if this predicate describes an equivalent language to S2
(eq-lang? student-language (language (language-alphabet student-language)
                                        (λ (w) (greater-than-k 2 0 w)) "check"))

;; counterexample lang
(define ce-lang (union-lang (set-diff-lang true-language student-language)
                            (set-diff-lang student-language true-language)))
;; predicate lang
(define morethan2 (language (list 0 1) (λ (w) (greater-than-k 2 0 w)) "more than 2 0s"))

;; k cannot be larger than the bound on our underlying language generator (8)
;; so k is bounded to 1-8 and s is bounded to the language alphabet
(define (search-k lang max-k pred)
  (for/first ([i (in-range 1 (add1 max-k))]
              #:when (eq-lang? lang (language (language-alphabet lang)
                                              (λ (w) ((curry pred i) w)) "test")))
    i))
    
(search-k ce-lang 8 (λ(k w) (greater-than-k k 0 w)))

;; search over both k and symbols
(define (search-k-symbol lang max-k pred)
  (for*/first ([s (language-alphabet lang)]
               [k (in-range 1 (add1 max-k))]
               #:when (eq-lang? lang (language (language-alphabet lang)
                                               (λ (w) ((curry pred k s) w)) "test")))
    (cons k s)))

;; thus we can check all values of k and symbols in sigma
;; for many different predicates
;; to synthesize a description
(define descriptions (list (cons "Words with at least ~a occurrences of '~a'"
                                 greater-eq-k)
                           (cons "Words with at most ~a occurrences of '~a'"
                                 less-eq-k)
                           (cons "Words with exactly ~a occurrences of '~a'"
                                 exactly-k)))
;;; these should be packaged as language structs




(define (synthesize-descriptions lang descriptions max-k)
  (for*/first ([d descriptions]
              [s (language-alphabet lang)]
              [k (in-range 1 (add1 max-k))]
              #:when (eq-lang? lang (language (language-alphabet lang)
                                              (curry (cdr d) k s) "test")))
   ;; (list (car d) k s)))
    (printf (car d) k s)))


;;; really filter over set of possible languages (symbolic language)
;;; looking for eq/subset/superset/etc to our specified lang

(define only-ones (language (list 0 1) (λ (w) (and (not (empty? w)) (not (empty? (word-value w)))
                                                   (andmap (λ (s) (eq? s 1)) (word-value w)))) "only ones"))
  
(define only-zeroes (language (list 0 1) (λ (w) (and (not (empty? w)) (not (empty? (word-value w)))
                                                     (andmap (λ (s) (eq? s 0)) (word-value w)))) "only zeroes"))

