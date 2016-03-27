#lang racket

(require racket/generator)
(require "automata.rkt")
(require "bruteforcemethods.rkt")

(define S2
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S2_trace
  (automaton2 s0
             [s0 : "s0" (0 → s1)
                 (1 → s0)]
             [s1 : "s1" (0 → s2)
                 (1 → s1)]
             [s2 : "s2" (0 → s3)
                 (1 → s2)]
             [s3 : "s3" (0 → s3)
                 (1 → s3)]))

(define T2
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s0)]
             [s1 : (accept : #f)
                 (0 → s2)
                 (1 → s1)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s2)]))

;; note that all of these mutation methods can produce
;; the same string multiple times

(define (mutate-del word)
  (generator ()
             (let loop ([lprime '()]
                        [ele (car word)]
                        [l (cdr word)])
               (if (empty? l)
                   (begin (yield lprime) '())
                   (begin
                     (yield (append lprime l))
                     (loop (append lprime (list ele)) (car l) (cdr l)))))))

(define (mutate-add word alphabet)
  (generator ()
             (begin
             (let loop ([lprime '()]
                        [l word])
               (if (empty? l)
                   (begin
                     (for ([a alphabet])
                       (yield (append lprime (list a))))
                     '())
                   (begin
                     (for ([a alphabet])
                       (yield (append lprime (list a) l)))
                     (loop (append lprime (list (car l))) (cdr l))))))))

(define (mutate-sub word alphabet)
  (generator ()
             (begin
             (let loop ([lprime '()]
                        [ele (car word)]
                        [l (cdr word)])
               (if (empty? l)
                   (begin
                     (for ([a alphabet] #:unless (eq? a ele))
                       (yield (append lprime (list a))))
                     '())
                   (begin
                     (for ([a alphabet] #:unless (eq? a ele))
                       (yield (append lprime (list a) l)))
                     (loop (append lprime (list ele)) (car l) (cdr l))))))))

;; all deletions, all additions, all substitutions
(define (mutate w alphabet)
  (generator ()
             (begin
               (unless (empty? w)
               (for ([wprime (in-producer (mutate-del w) null)])
                 (yield wprime)))
               (for ([wprime (in-producer (mutate-add w alphabet) null)])
                 (yield wprime))
               (unless (empty? w)
               (for ([wprime (in-producer (mutate-sub w alphabet) null)])
                 (yield wprime)))
               null)))

; determine if adding 1, deleting 1, or mutating 1 produces the longest list
; it's not ever going to be deleting 1, but ok
(define (longest-mutation word alphabet)
  (max (* (add1 (length word)) (length alphabet))
       (length word)
       (* (length word) (sub1 (length alphabet)))))

;; interleave deletions/additions/substitutions
(define (mutate2 w alphabet)
  (generator ()
             (let ([addgen (mutate-add w alphabet)]
                   [delgen (if (empty? w) (infinite-generator (yield '()))
                               (mutate-del w))]
                   [subgen (if (empty? w) (infinite-generator (yield '()))
                               (mutate-sub w alphabet))])
               (begin
               (for ([i  (in-range (longest-mutation w alphabet))])
                 (let ([addword (addgen)]
                       [delword (delgen)]
                       [subword (subgen)])
                   (unless (empty? addword) (yield addword))
                   (unless (empty? delword) (yield delword))
                   (unless (empty? subword) (yield subword))))
               '()))))

(define (exists-one-away wss T w)
  (λ (S)
    (for/first ([w (in-producer (mutate2 w (word-search-space-alphabet wss)) null)]
                #:when (same-outcome? T S w))
      (if w (word w) w))))

(define (exists-small-multiple wss T)
  (λ (S)
    (for*/first ([w (in-producer (words (word-search-space-alphabet wss) (word-search-space-k wss)) null)]
                [wprime (list ((exists-one-away wss T (word-value w)) S))]
                #:when (and (not (same-outcome? S T (word-value w))) wprime))
      (list w wprime))))

(define x (exists-small-multiple (word-search-space (list 0 1) 5) T2))
(define r (x S2))
(define bad-val (word-value (car r)))
(define good-val (word-value (cadr r)))

(define (find-state-pair M1 M2 word current-state)
  (if (same-outcome? M1 M2 word)
      (list (cons current-state "green"))
      (list (cons current-state "red"))))

(define (code-states M1 M2 trace word)
  (if (eq? (length trace) (add1 (length word)))
      (letrec ([f (lambda (w wp tr trmap)
                    (if (empty? w)
                        (append trmap (find-state-pair M1 M2 wp (car tr)))
                        (f (cdr w) (append wp (list (car w))) (cdr tr) (append trmap (find-state-pair M1 M2 wp (car tr))))))])
        (f word '() trace '()))
      null))

;(code-states S2 T2 (S2_trace bad-val '()) bad-val)
;(code-states S2 T2 (S2_trace good-val '()) good-val)
  
               

  
  