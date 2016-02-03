#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

(provide solve-automaton-ce solve-split-state exists-word exists-word-forall-words exists-word-exists-word)

(define (solve-automaton-ce m1 m2 alphabet k)
  (define w (symbolic-word* k alphabet))
  (evaluate w (solve (assert (not (same-outcome? m1 m2 w))))))

(define (exists-word M1 M2 alphabet k predicate)
  (define w (symbolic-word* k alphabet))
  (define model
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (solve (assert (predicate M1 M2 w)))))
  (if (empty? model) null (word (evaluate w model))))

(define (solve-split-state m1 m2 alphabet k)
  (define w (symbolic-word* k alphabet))
  (define wprime (symbolic-word* k alphabet))
  (m1 (evaluate w (solve (begin (assert (eq? (m1 w) (m1 wprime)))
                (assert (not (eq? (m2 w) (m2 wprime)))))))))

(define (exists-word-forall-words M1 M2 alphabet k predicate)
  (define w (symbolic-word* k alphabet))
  (define wprime (symbolic-word* k alphabet))
  (define binding
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (synthesize #:forall (list wprime)
                  #:guarantee (assert (predicate M1 M2 w wprime)))))
  (if (empty? binding) null (word (evaluate w))))

(define (exists-word-exists-word M1 M2 alphabet k predicate)
  (define w (symbolic-word* k alphabet))
  (define wprime (symbolic-word* k alphabet))
  (define model
    (with-handlers ([exn:fail? (lambda (ex) null)])
      (solve (assert (predicate M1 M2 w wprime)))))
  (if (empty? model) null (list (word (evaluate w model)) (word (evaluate wprime model)))))