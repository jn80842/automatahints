#lang s-exp rosette

(require 
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(require "automata.rkt")

(provide exists-word exists-word-forall-words exists-word-exists-word)

(define (words alphabet [k 100])
  (symbolic-word* k alphabet))

(define (exists-word wss T predicate)
  (λ (S)
    (define w (words (word-search-space-alphabet wss) (word-search-space-k wss)))
    (define model
      (with-handlers ([exn:fail? (lambda (ex) null)])
        (solve (assert (predicate S T w)))))
      (if (empty? model) null (word (evaluate w model)))))

(define (exists-word-forall-words wss T predicate)
  (λ (S)
    (define w (words (word-search-space-alphabet wss) (word-search-space-k wss)))
    (define wprime (words (word-search-space-alphabet wss) (word-search-space-k wss)))
    (define binding
      (with-handlers ([exn:fail? (λ (ex) null)])
        (synthesize #:forall (list wprime)
                    #:guarantee (assert (predicate S T w wprime)))))
    (if (empty? binding) null (word (evaluate w)))))

(define (exists-word-exists-word wss T predicate)
  (λ (S)
    (define w (words (word-search-space-alphabet wss) (word-search-space-k wss)))
    (define wprime (words (word-search-space-alphabet wss) (word-search-space-k wss)))
    (define model
      (with-handlers ([exn:fail? (λ (ex) null)])
        (solve (assert (predicate S T w wprime)))))
    (if (empty? model) null (list (word (evaluate w model)) (word (evaluate wprime model))))))
