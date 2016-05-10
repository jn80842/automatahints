#lang racket

;; language can be defined by a DFA, a regular expression,
;; a generator, or a set of predicates over an alphabet

(define student-language (language student-dfa))
(define correct-language (language correct-dfa))
(define correct-language2 (language alphabet (λ (w) (contains-substring? "ab" w))))
(define sigma-star-language (language alphabet (λ (w) (true))))

;; languages can be combined using set operators

(define counterexample-language (union (set-diff student-language correct-language)
                                       (set-diff correct-language student-language)))
(define correctly-classified-language (intersection student-language correct-language))

;; languages can be treated as generators
(define counterexample-gen (language->generator counterexample-language))

;; use languages to define stream of tuples, where each element of tuple satisfies a function
(define small-multiples-tuples (tuple-generator (λ (w wprime) (> 2 (edit-distance w wprime)))
                                           counterexample-language (intersection student-language correct-language)))



