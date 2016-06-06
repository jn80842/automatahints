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


;; attributes of languages
(non-empty? L1)
(finite? L1)
(infinite? L1)
(subset? L1 L2)

;; description predicate that should be true for every word in the language
;; predicates map to English description
(λ (w) (prefix? ?? w))
(λ (w) (substring? ?? w))
(λ (w) (eq? ?? (length w)))
(λ (w) (even? (length w)))
(λ (w) (odd? (length w)))
(λ (w) (<= ?? (count-symbol ?? w)))


(define prefix-lang (subset? (language alphabet (λ (w) (prefix-append wprime w))) counterexample-language))
(equal? prefix-lang (language alphabet (λ (w) (<= ?? (count-symbol ?? w)))))


;; from mtg -- synthesize subset of lang that contains some substring
(define LL (map (λ (s) (language alphabet (λ (w) (substring? s w)))) (list ?? ?? ??)))

(define lang-constraints (and (subset? L1 counterexample-language)
                              (infinite? L1)
                              (forall-words (λ (w) (substring? ?? w)) L1)
                              (empty? (intersection L1 correct-language))))





