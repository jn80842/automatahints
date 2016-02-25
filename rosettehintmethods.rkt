#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

(provide solve-automaton-ce solve-split-state exists-word exists-word-forall-words exists-word-exists-word)

(define (solve-automaton-ce M1 M2)
  (define sigma (alphabet M2))
  (define k (* (length (states M1)) (length (states M2))))
  (define w (symbolic-word* k sigma))
  (evaluate w (solve (assert (not (same-outcome? M1 M2 w))))))

(define (exists-word M1 M2 predicate)
  (define sigma (alphabet M2))
  (define k (* (length (states M1)) (length (states M2))))
  (define w (symbolic-word* k sigma))
  (define model
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (solve (assert (predicate M1 M2 w)))))
  (if (empty? model) null (word (evaluate w model))))

(define (solve-split-state M1 M2)
  (define sigma (alphabet M2))
  (define k (* (length (states M1)) (length (states M2))))
  (define w (symbolic-word* k sigma))
  (define wprime (symbolic-word* k sigma))
  (M1 (evaluate w (solve (begin (assert (eq? (M1 w) (M1 wprime)))
                (assert (not (eq? (M2 w) (M2 wprime)))))))))

(define (exists-word-forall-words M1 M2 predicate)
  (define sigma (alphabet M2))
  (define k (* (length (states M1)) (length (states M2))))
  (define w (symbolic-word* k sigma))
  (define wprime (symbolic-word* k sigma))
  (define binding
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (synthesize #:forall (list wprime)
                  #:guarantee (assert (predicate M1 M2 w wprime)))))
  (if (empty? binding) null (word (evaluate w))))

(define (exists-word-exists-word M1 M2 predicate)
  (define sigma (alphabet M2))
  (define k (* (length (states M1)) (length (states M2))))
  (define w (symbolic-word* k sigma))
  (define wprime (symbolic-word* k sigma))
  (define model
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (solve (assert (predicate M1 M2 w wprime)))))
  (if (empty? model) null (list (word (evaluate w model)) (word (evaluate wprime model)))))