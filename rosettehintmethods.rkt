#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

(provide words exists-word exists-word-forall-words exists-word-exists-word)

(define (words alphabet [k 100])
  (symbolic-word* k alphabet))

(define (exists-word source T predicate)
  (λ (S)
    (define model
      (with-handlers ([exn:fail? (lambda (ex) null)])
        (solve (assert (predicate S T source)))))
      (if (empty? model) null (word (evaluate source model)))))

(define (exists-word-forall-words source source2 T predicate)
  (λ (S)
    (define binding
      (with-handlers ([exn:fail? (λ (ex) null)])
        (synthesize #:forall (list source2)
                    #:guarantee (assert (predicate S T source source2)))))
    (if (empty? binding) null (word (evaluate source)))))

(define (exists-word-exists-word source source2 T predicate)
  (λ (S)
    (define model
      (with-handlers ([exn:fail? (λ (ex) null)])
        (solve (assert (predicate S T source source2)))))
    (if (empty? model) null (list (word (evaluate source model)) (word (evaluate source2 model))))))

;(define (exists-word-gen source T predicate)
;  (λ (S)
;    (generator ()
;               (begin
;                 (let loop
