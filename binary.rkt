#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sipser Binary Problems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.32 Sigma_{3} st. first 2 rows add up to bottom row (reversed)

(define T132R
  (automaton q0
             [q0 : accept ("000" → q0)
                  ("101" → q0)
                  ("011" → q0)
                  ("110" → q1)
                  ("100" → q2)
                  ("010" → q2)
                  ("001" → q2)
                  ("111" → q2)]
             [q1 : ("001" → q0)
                 ("000" → q2)
                  ("111" → q1)
                  ("010" → q1)
                  ("100" → q1)
                  ("110" → q2)
                  ("011" → q2)
                  ("101" → q2)]
             [q2 : ("000" → q2)
                  ("001" → q2)
                  ("010" → q2)
                  ("011" → q2)
                  ("100" → q2)
                  ("101" → q2)
                  ("110" → q2)
                  ("111" → q2)]))

(define T132
  (automaton q0
             [q0 : accept ("000" → q0)
                  ("101" → q0)
                  ("011" → q0)
                  ("110" → q2)
                  ("100" → q2)
                  ("010" → q2)
                  ("001" → q1)
                  ("111" → q2)]
             [q1 : ("001" → q2)
                 ("000" → q2)
                  ("111" → q1)
                  ("010" → q1)
                  ("100" → q1)
                  ("110" → q0)
                  ("011" → q2)
                  ("101" → q2)]
             [q2 : ("000" → q2)
                  ("001" → q2)
                  ("010" → q2)
                  ("011" → q2)
                  ("100" → q2)
                  ("101" → q2)
                  ("110" → q2)
                  ("111" → q2)]))

(define S132
  (automaton q0
             [q0 : accept ("000" → q0)
                  ("101" → q0)
                  ("011" → q0)
                  ("110" → q1)
                  ("100" → q1)
                  ("010" → q2)
                  ("001" → q2)
                  ("111" → q2)]
             [q1 : ("001" → q0)
                 ("000" → q1)
                  ("111" → q1)
                  ("010" → q1)
                  ("100" → q1)
                  ("110" → q2)
                  ("011" → q2)
                  ("101" → q2)]
             [q2 : ("000" → q2)
                  ("001" → q2)
                  ("010" → q2)
                  ("011" → q2)
                  ("100" → q2)
                  ("101" → q2)
                  ("110" → q2)
                  ("111" → q2)]))

;; Sipser 1.33 bottom row equals top row x 3, reversed
(define T133
  (automaton q0
             [q0 : accept ("00" → q0)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q1)]
             [q1 : ("00" → sink)
                 ("01" → q2)
                 ("10" → q3)
                 ("11" → q1)]
             [q2 : accept ("00" → q2)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q1)]
             [q3 : ("00" → q4)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q3)]
             [q4 : ("00" → sink)
                 ("01" → q2)
                 ("10" → q3)
                 ("11" → sink)]
             [sink : ("00" → sink)
                   ("01" → sink)
                   ("10" → sink)
                   ("11" → sink)]))
;; Sipser 1.34 Top row is larger than bottom row

(define T134
  (automaton q0
             [q0 : accept ("00" → q0)
                 ("01" → q2)
                 ("10" → q1)
                 ("11" → q0)]
             [q1 : accept ("00" → q1)
                 ("01" → q1)
                 ("10" → q1)
                 ("11" → q1)]
             [q2 : ("00" → q2)
                 ("01" → q2)
                 ("10" → q2)
                 ("11" → q2)]))
(define S134
  (automaton q0
             [q0 : accept ("00" → q0)
                 ("01" → q1)
                 ("10" → q2)
                 ("11" → q0)]
             [q1 : accept ("00" → q1)
                 ("01" → q1)
                 ("10" → q1)
                 ("11" → q1)]
             [q2 : ("00" → q2)
                 ("01" → q2)
                 ("10" → q2)
                 ("11" → q2)]))

(define sigma3 (list "000" "001" "010" "011" "100" "101" "110" "111"))
(define sigma2 (list "00" "01" "10" "11"))

;; interpret string of sigma_3 as adding 2 rows of binary numbers
(define s3 (list "001" "111" "110"))
(define s3_2 (list "001" "100" "000"))

(define b3 (list "#b" "#b" "#b"))
(define b2 (list "#b" "#b"))

(define (test-binary-wordlist wl pred)
  (let ([initl (build-list (string-length (car wl)) (lambda (x) "#b"))])
  (let ([not-empty-string? (lambda (s) (> (string-length s) 0)) ])
    (let ([split-filter (lambda (s) (filter not-empty-string? (string-split s "")))])
      (let ([final-terms (foldl (lambda (s l) (map string-append l (split-filter s))) initl wl)])
          (pred (map string->number final-terms)))))))

(define (translate-binary-word wl)
  (let ([initl (build-list (string-length (car wl)) (lambda (x) "#b"))])
    (let ([not-empty-string? (lambda (s) (> (string-length s) 0)) ])
    (let ([split-filter (lambda (s) (filter not-empty-string? (string-split s "")))])
      (let ([final-terms (foldl (lambda (s l) (map string-append l (split-filter s))) initl wl)])
          (map string->number final-terms))))))

(define (add-pred l) (eq? (+ (car l) (cadr l)) (caddr l)))
(define (times3-pred l) (eq? (* (car l) 3) (cadr l)))
(define (greater-pred l) (> (car l) (cadr)))

;;; check that true solutions are correct up to Sigma^k
(define (check-sigma3-add M k)
  (define w (word* k sigma3))
  (evaluate w (solve (assert (not (eq? (M w) (test-binary-wordlist w add-pred)))))))

(define (check-sigma2-mult3 M k)
  (define w (word* k sigma2))
  (evaluate w (solve (assert (not (eq? M w) (test-binary-wordlist w times3-pred))))))

(define (check-sigma2-greater M k)
  (define w (word* k sigma2))
  (evaluate w (solve (assert (not (eq? M w) (test-binary-wordlist w greater-pred))))))

;;;; counterexample on 2-col greater than ;;;;
(define (same-outcome? m1 m2 w)
  (eq? (m1 w) (m2 w)))
(printf "Counterexample hint:\n")
(define (solve-automaton-ce m1 m2 k)
  (define w (word* k '("00" "01" "10" "11")))
  (evaluate w (solve (assert (not (same-outcome? m1 m2 w))))))

(define add-ce (solve-automaton-ce S134 T134 4))

(define translatedterm (translate-binary-word add-ce))
(printf "Consider the terms ~a and ~a.\n\n" (car translatedterm) (car (cdr translatedterm)))