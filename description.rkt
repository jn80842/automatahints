#lang racket

(require racket/generator)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")
(require "descriptionhelpers.rkt")

;; a language is defined by a filtering predicate over the set Sigma* (or a bounded Sigma^k).
(struct language (alphabet predicate description))

;; a DFA is just another kind of predicate
(define student-language (language (list 0 1) (λ (w) (S2 (word-value w))) "The language defined by the student solution DFA"))
(define true-language (language (list 0 1) (λ (w) (T2 (word-value w))) "The language defined by the correct solution DFA"))

;; we can make a generator to emit all members of the language
(define (language-generator lang)
  (generator ()
             (begin
               (for ([w (in-producer (words (language-alphabet lang) 8))]
                     #:break (empty? w)
                     #:when ((language-predicate lang) w))
                 (yield w)))
             null))

;; we can compose languages with set operators
;; assume they share an alphabet
(define (union-lang lang1 lang2)
  (language (language-alphabet lang1) (λ (w) (or ((language-predicate lang1) w) ((language-predicate lang2) w)))
            (string-append "union " (language-description lang1) " and " (language-description lang2))))
(define (intersect-lang lang1 lang2)
  (language (language-alphabet lang1) (λ (w) (and ((language-predicate lang1) w) ((language-predicate lang2) w)))
            (string-append "intersect " (language-description lang1) " and " (language-description lang2))))
(define (set-diff-lang lang1 lang2)
  (language (language-alphabet lang1) (λ (w) (and ((language-predicate lang1) w) (not ((language-predicate lang2) w))))
            (string-append "set diff " (language-description lang1) " and " (language-description lang2))))
(define (set-complement-lang lang)
  (language (language-alphabet lang) (λ (w) (not ((language-predicate lang) w))) (string-append "complement " (language-description lang))))

;; compose two DFA-based languages to get a language of counterexamples
(define counterexample-lang (union-lang (set-diff-lang student-language true-language) (set-diff-lang true-language student-language)))

;; we want to synthesize a language defined by a predicate like this one
;; that's equivalent to the student solution (or equivalent to the counterexample language)
;; in order to get a description like "All words with more than k occurrences of s
(define (greater-than-k k symbol w)
  (if (empty? w)
      0
      (< k (count (λ (s) (eq? symbol s)) (word-value w)))))

;; we can check two languages for equivalency
;; assumption that languages are ordered in the same way
;; note: this will be wrong if lang1 and lang2 are equivalent but of different length
(define (eq-lang? lang1 lang2)
  (empty? (for/first ([w (in-producer (language-generator lang1))]
              [wprime (in-producer (language-generator lang2))]
              #:when (not (equal-word? w wprime)))
    1)))

;; if we pick k=2, symbol=0, we can check if this predicate describes an equivalent language to S2
(eq-lang? student-language (language (language-alphabet student-language)
                                        (λ (w) (greater-than-k 2 0 w)) "check"))

;; k cannot be larger than the bound on our underlying language generator (8)
;; so k is bounded to 1-8 and s is bounded to the language alphabet
(define (search-k lang max-k pred)
  (for/first ([i (in-range 1 (add1 max-k))]
              #:when (eq-lang? lang (language (language-alphabet lang) (λ (w) ((curry pred i) w)) "test")))
    i))
    

;; thus we can check all values of k and symbols in sigma
;; for many different predicates
;; to synthesize a description
(define descriptions (list (cons "Words with at least ~a occurrences of ~a"
                                 (λ (k s w) (<= k (count (λ(x) (eq? s x)) w))))
                           (cons "Words with at most ~a occurrences of ~a"
                                 (λ (k s w) (>= k (count (λ(x) (eq? s x)) w))))
                           (cons "Words with exactly ~a occurrences of ~a"
                                 (λ (k s w) (eq? k (count (λ(x) (eq? s x)) w))))))

;; we can also synthesize languages that are a subset of the target language
(define (word-in-lang? w lang)
  ((language-predicate lang) w))

(define (subset-lang? lang1 lang2)
  (let ([r (for/first ([w (in-producer (language-generator lang1))]
                       #:break (empty? w)
                       #:when (not (word-in-lang? w lang2)))
             w)])
    (not (word? r))))





