#lang racket
(require racket/generator)

(require "automata.rkt")
(require "descriptionhelpers.rkt")

(provide union-lang
         intersect-lang
         set-diff-lang
         set-complement-lang
         concat-lang
         word-in-lang?
         empty-lang?
         eq-lang?
         subset-lang?
         disjoint-lang?)

;; composing languages always assumes that they share an alphabet

;; compose languages with set operators

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
  (language (language-alphabet lang) (λ (w) (not ((language-predicate lang) w)))
            (string-append "complement " (language-description lang))))

;; compose languages with regular language closure properties

(define (concat-lang lang1 lang2)
  (language (language-alphabet lang1) (λ (w) (ormap (λ (i) ((and ((language-predicate lang1) (word (take (word-value w) i)))
                                                                 ((language-predicate lang2) (word (drop (word-value w) i))))))
                                                    (range 0 (length (word-value w))))) (string-append "concat " (language-description lang1)
                                                                                                       " and " (language-description lang2))))

;; predicates for languages

(define (empty-lang? lang)
  (let ([lg (language-generator lang)])
    (eq? '() (lg))))

;; we can check two languages for equivalency
;; assumption that languages are ordered in the same way
;; note: this will be wrong if lang1 and lang2 are equivalent but of different length
(define (eq-lang? lang1 lang2)
  (not (for/first ([w (in-producer (language-generator lang1))]
              [wprime (in-producer (language-generator lang2))]
              #:break (and (empty? w) (empty? wprime))
              #:when (not (equal-word? w wprime)))
    #t)))

;; we can also synthesize languages that are a subset of the target language
(define (word-in-lang? w lang)
  ((language-predicate lang) w))

(define (subset-lang? lang1 lang2)
  (let ([r (for/first ([w (in-producer (language-generator lang1))]
                       #:break (empty? w)
                       #:when (not (word-in-lang? w lang2)))
             w)])
    (not (word? r))))

(define (disjoint-lang? lang1 lang2)
  (let* ([allwords (language (language-alphabet lang1) (λ(w) #t) "all words")]
         [r (for/first ([w (in-producer (language-generator allwords))]
                #:break (empty? w)
                #:when (and (word-in-lang? w lang1) (word-in-lang? w lang2))) w)])
    (not (word? r))))