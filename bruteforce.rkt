#lang racket

(require racket/engine)

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")

;; fake correctness checker, since all our examples are wrong
(define (correct-check S) #f)

;;;; counterexample ;;;;;
(printf "Counterexample hint:\n")

(define word-gen (words (alphabet T))) ;; unbounded word generator

(define counterexample-generator
  (exists-word-f word-gen T counterexample-pred))

(define (display-hint hint-generator)
  (lambda (S)
    (if (correct-check S)
        (printf "Correct.\n\n")
        (let ([hint (hint-generator S)])
          (if (empty? hint)
              (printf "No counterexample could be found.\n\n")
              (printf "The word ~a is a counterexample.\n\n" (word-value hint)))))))

(define counterexample-hint (display-hint counterexample-generator))

(counterexample-hint S)

(define (hint-e hint-generator)
  (lambda (S)
    (engine
   (λ (_)
     (hint-generator S)))))

(define (get-result eng [timeout 10000])
  (engine-run timeout eng)
  (engine-result eng))

(define (display2 correct? hint-engine)
  (lambda (S)
    (if (correct? S)
        (printf "Your solution is correct.\n\n")
        (let ([hint-result (get-result (hint-engine S))])
          (if (word? hint-result)
              (printf "The word ~a is a counterexample.\n\n" (word-value hint-result))
              (printf "No counterexample could be found.\n\n"))))))

(define he (hint-e counterexample-generator))
(define hint2 (display2 correct-check he))


(define e (engine
           (λ (_)
             (let loop ()
               (displayln "hi")
               (sleep 1)
               (loop)))))

;;;;;; prefix hint ;;;;;;
; for some prefix p, for all words w of length less than k, p.w will have a different outcome on m1 and m2
; we want to synthesize p

(printf "Prefix hint:\n")
(define prefix (exists-word-forall-words S2 T2 bad-prefix-pred))
(if (empty? prefix)
    (printf "No prefix of length ~a or less was found.\n\n" 3)
    (printf "The prefix ~a st. p followed by any word up to length ~a will not have the desired behavior.\n\n" (word-value prefix) 3))

;;;;;;; split state hint ;;;;;;;
;; split states
; some state s in S where strings that arrive in s are both accepted and rejected by T

(printf "\nSplit state hint:")

(define split-state-words (exists-word-exists-word S3 T3 split-state-pred))

(if (empty? split-state-words)
    (printf "\nNo split state was found when checking words up to ~a in length.\n\n" 10)
    (printf "\nWords that arrive in the state ~a have different behaviors on the true solution.\n\n" (S3 (word-value (car split-state-words)))))

;;;;;;;; sequence of counterexamples ;;;;;;
;; sequence 1: get arbitrary number of counterexamples
;; sequence 2: get arbitrary number of counterexamples that match some pattern



;;;;;;;; small multiples ;;;;;;
;; get counterexample, then get word with good outcome that has a close edit distance from counterexample
