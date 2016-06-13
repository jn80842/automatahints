#lang s-exp rosette/safe

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta rosette/lib/reflect/lift)

(require (only-in racket [string->number racket/string->number]
                  [string-length racket/string-length]
                  [string-split racket/string-split]
                  string?))

(require "automata.rkt")
;(require "rosettehintmethods.rkt")
(require "bruteforcemethods.rkt")

(define-lift string->number [(string?) racket/string->number])
(define-lift string-length [(string?) racket/string-length])
(define-lift string-split [(string? string?) racket/string-split])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sipser Binary Problems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.32 Sigma_{3} st. first 2 rows add up to bottom row (reversed)

(define T132
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

; with state names for split state hint
; ideally we would not need to change the formatting? idk
(define S133
  (automaton2 q0
             [q0 : "q0" ("00" → q0)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q1)]
             [q1 : "q1" ("00" → sink)
                 ("01" → q3)
                 ("10" → q2)
                 ("11" → q1)]
             [q2 : "q2" ("00" → q2)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q1)]
             [q3 : "q3" ("00" → q4)
                 ("01" → sink)
                 ("10" → sink)
                 ("11" → q3)]
             [q4 : "q4" ("00" → sink)
                 ("01" → q2)
                 ("10" → q3)
                 ("11" → sink)]
             [sink : "sink" ("00" → sink)
                   ("01" → sink)
                   ("10" → sink)
                   ("11" → sink)]))
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

;;; alternative method of interpreting strings as binary numbers
;;; commented out as they contain unlifted string methods

;(define (test-binary-wordlist initl wl pred)
;  (let ([not-empty-string? (lambda (s) (> (string-length s) 0)) ])
;    (let ([split-filter (lambda (s) (filter not-empty-string? (string-split s "")))])
;      (let ([final-terms (foldl (lambda (s l) (map string-append l (split-filter s))) initl wl)])
;          (pred (map string->number final-terms))))))
          
; (define (translate-binary-word initl wl)
;    (let ([not-empty-string? (lambda (s) (> (string-length s) 0)) ])
;    (let ([split-filter (lambda (s) (filter not-empty-string? (string-split s "")))])
;      (let ([final-terms (foldl (lambda (s l) (map string-append l (split-filter s))) initl wl)])
;          (map string->number final-terms)))))

(define (sigma-n->decimal str lst)
  (let ([twolst (map (lambda (x) (* 2 x)) lst)]
        [newlst (map string->number (filter (lambda (s) (> (string-length s) 0)) (string-split str "")))])
    (map (lambda (one two) (+ one two)) twolst newlst)))

(define (add-pred l) (eq? (+ (car l) (cadr l)) (caddr l)))
(define (times3-pred l) (eq? (* (car l) 3) (cadr l)))
(define (greater-eq-pred l) (>= (car l) (cadr l)))

(define (sigma3->decimal word)
  (foldl sigma-n->decimal (list 0 0 0) word))

;;;; counterexample on 2-col greater than ;;;;

(printf "Counterexample hint:\n")
(define ce (exists-word S134 T134 sigma2 4 counterexample-pred))
(if (empty? ce)
    (printf "No counterexample of size ~a of less was found.\n\n" 3)
    (printf "The word ~a is a counterexample.\n\n" (word-value ce)))

(define (solve-check-pred m1 alphabet k predicate)
  (define w (symbolic-word* k alphabet))
  (evaluate w (solve (assert (not (eq? (m1 w) (predicate (foldl sigma-n->decimal (list 0 0) w))))))))
(time 
(define greater-pred-ce (solve-check-pred S134 sigma2 3 greater-eq-pred))
(printf "The word ~a is a counterexample for automaton S134 (found via Rosette using predicate).\n" (evaluate greater-pred-ce)))
(printf "\n")

(printf "Prefix hint:\n")

(time
(define (prefixer w)
  (define p (list (list-ref sigma2 (??))
                  (list-ref sigma2 (??))
                  (list-ref sigma2 (??))))
  (append p w))
(define (prefix-check m1 m2 prefixer w)
  (assert (not (same-outcome? m1 m2 (prefixer w)))))
(define wprime (symbolic-word* 3 sigma2))
(define binding
  (synthesize #:forall (list wprime)
              #:guarantee (prefix-check S134 T134 prefixer wprime)))
(print-forms binding))

(time (printf "The prefix ~a st. p followed by any word up to length k will not have the desired behavior (found via enumerative search).\n" (find-failing-prefix S134 T134 sigma2 3)))
(printf "\n")
;;;; split state on 2nd row = 1st row * 3 ;;;;
(printf "Split state hint:\n")

;(time (printf "Words that arrive in state ~a have different behaviors on the true solution (found via Rosette).\n" (solve-split-state S133 T133 sigma2 4)))
;(time (printf "Words that arrive in state ~a have different behaviors on the true solution (found via enumerative search).\n" (car (find-split-state S133 T133 sigma2 4))))


