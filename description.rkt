#lang racket

(require racket/generator)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")

;; a language is defined by a filtering predicate over the set Sigma* (or a bounded Sigma^k).
(struct language (alphabet predicate description))

;; a DFA is just another kind of predicate
(define student-language (language (list 0 1) S2 "The language defined by the student solution DFA"))
(define true-language (language (list 0 1) T2 "The language defined by the correct solution DFA"))

;; we can make a generator to emit all members of the language
(define (language-generator lang)
  (generator ()
             (begin
               (for ([w (in-producer (words (language-alphabet lang) 8))]
                     #:break (empty? w)
                     #:when ((language-predicate lang) (word-value w)))
                 (yield w)))
             null))

;; we want to synthesize a language defined by a predicate like this one
;; that's equivalent to the student solution (or equivalent to the counterexample language)
;; in order to get a description like "All words with more than k occurrences of s
(define (greater-than-k k symbol w)
  (< k (count (λ (s) (eq? symbol s)) w)))

;; we can check two languages for equivalency
(define (eq-lang? lang1 lang2)
  (empty? (for/first ([w (in-producer lang1)]
              [wprime (in-producer lang2)]
              #:when (not (eq? w wprime)))
    1)))

;; if we pick k=2, symbol=0, we can check if this predicate describes an equivalent language to S2
(eq-lang? (language-generator student-language)
          (language-generator (language (list 0 1) (λ (w) (greater-than-k 2 0 w)) "check")))

;; thus we can check all values of k and symbols in sigma
;; for many different predicates
;; to synthesize a description
(define descriptions (list (cons "Words with at least ~a occurrences of ~a" (λ (k s w) (<= k (count (λ(x) (eq? s x)) w))))
                           (cons "Words with at most ~a occurrences of ~a" (λ (k s w) (>= k (count (λ(x) (eq? s x)) w))))
                           (cons "Words with exactly ~a occurrences of ~a" (λ (k s w) (eq? k (count (λ(x) (eq? s x)) w))))))

;; we can also synthesize languages that are a subset of the target language
(define (subset-lang? lang1 lang2 alphabet)
  (letrec ([f (λ (w wprime)
                (cond [(empty? w) #t]
                      [(empty? wprime) #f]
                      [(equal-word? (word-value w) (word-value wprime)) (f (lang1) (lang2))]
                      [(greater-word? (word-value w) (word-value wprime) alphabet) (f w (lang2))]
                      [(lesser-word? (word-value w) (word-value wprime) alphabet) #f]))])
    (f (lang1) (lang2))))


;;;;;;;;;;;;;;;;;;;
;;;;; helpers ;;;;;
;;;;;;;;;;;;;;;;;;;

;; null words aren't equal to anything
(define (equal-word? w wprime)
  (and (not (empty? w))
       (and (not (empty? wprime))
            (and (eq? (length w) (length wprime))
                 (andmap eq? w wprime)))))

;; assume all alphabets are integers for now; can map symbols to ints later
(define (word->decimal word alphabet)
  (let ([base (length alphabet)])
    (foldl (λ (digit place sum) (+ sum (* digit (expt base place)))) 0 word (reverse (range 0 (length word))))))
      
(define (word->ordinal word alphabet)
  (let* ([base (length alphabet)]
        [prior (foldl + 0 (map (λ (x) (expt base x)) (range 0 (length word))))]
        [current (word->decimal word alphabet)])
    (+ prior current)))

(define (greater-word? w wprime alphabet)
  (> (word->ordinal w alphabet) (word->ordinal wprime alphabet)))
(define (lesser-word? w wprime alphabet)
  (< (word->ordinal w alphabet) (word->ordinal wprime alphabet)))


