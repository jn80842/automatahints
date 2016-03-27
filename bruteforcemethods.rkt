#lang racket

(require racket/generator)
(require "automata.rkt")

(provide wordgenerator words
         exists-word forall-words
         exists-word-forall-words exists-word-exists-word)

(define (extend-word-by-one alphabet prevlist)
  (let ([lists (for/list ([i alphabet])
    (map (lambda (l) (cons i l)) prevlist))])
    (append* lists)))

(define (wordgenerator alphabet)
  (generator ()
             (begin
               (yield '())
               (let loop ([workinglst (map (lambda (x) (list x)) alphabet)]
                          [originallst (map (lambda (x) (list x)) alphabet)])
                 (if (empty? workinglst)
                     (loop (extend-word-by-one alphabet originallst) (extend-word-by-one alphabet originallst))
                     (begin
                       (yield (car workinglst))
                       (loop (cdr workinglst) originallst)))))))

(define (words alphabet [k 100])
  (generator ()
             (begin
               (yield (word '()))
               (let loop ([workinglst (map (lambda (x) (list x)) alphabet)]
                          [originallst (map (lambda (x) (list x)) alphabet)]
                          [limit (words-up-to-k (length alphabet) k)]
                          [i 0])
                 (if (eq? limit i)
                     null
                     (if (empty? workinglst)
                         (loop (extend-word-by-one alphabet originallst) (extend-word-by-one alphabet originallst) limit (add1 i))
                         (begin
                           (yield (word (car workinglst)))
                           (loop (cdr workinglst) originallst limit (add1 i)))))))))

(define (words-up-to-k alphabet-length k)
  (for/sum ([i (in-range (+ k 1))]) (expt alphabet-length i)))

(define (exists-word wss T predicate)
  (λ (S)
    (for/first ([w (in-producer (words (word-search-space-alphabet wss) (word-search-space-k wss)) null)]
                #:when (or (empty? w) (predicate S T (word-value w))))
      w)))

(define (exists-word-gen wss T predicate)
  (λ (S)
    (generator ()
               (begin
                 (for ([w (in-producer (words (word-search-space-alphabet wss) (word-search-space-k wss)))]
                       #:when (or (empty? w) (predicate S T (word-value w))))
                   (yield w))))))

; test if some predicate is true for all words wrt some word w
; iterate, find some wprime that makes predicate false and return #f
; or hit end of sequence and return #t
(define (forall-words gen S T predicate w)
  (for/first ([wprime (in-producer gen)]
              #:when (or (empty? wprime) (not (predicate S T (word-value w) (word-value wprime)))))
       (empty? wprime)))

(define (exists-word-forall-words wss T predicate)
  (λ (S)
    (for/first ([w (in-producer (words (word-search-space-alphabet wss) (word-search-space-k wss)))]
                #:when (forall-words (words (word-search-space-alphabet wss) (word-search-space-k wss)) S T predicate w))
      w)))

(define (exists-word-exists-word wss T predicate)
  (λ (S)
    (for*/first ([w (in-producer (words (word-search-space-alphabet wss) (word-search-space-k wss)))]
                 [wprime (list ((exists-word wss T (λ (S1 T1 w1) (predicate S1 T1 (word-value w) w1))) S))]
                 #:when (or (word? wprime) (and (empty? w) (empty? wprime))))
      (list w wprime))))

;;;;;;;; generator letrec methods ;;;
(define (exists-word-letrec M1 M2 alphabet k predicate)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) "no such word found"]
                          [(predicate M1 M2 w) (word w)]
                          [else (f (add1 i))])))])
      (f 1))))

(define (forall-words-letrec M1 M2 alphabet k predicate w)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (cond [(eq? i limit) #t] ; predicate holds for all words up to limit
                        [(not (predicate M1 M2 w (gen))) #f] ; predicate is false for at least 1 word
                        [else (f (add1 i))]))])
      (f 1))))

(define (exists-word-forall-words-letrec M1 M2 alphabet k predicate)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) null]
                          [(forall-words M1 M2 alphabet k predicate w) (word w)]
                          [else (f (add1 i))])))])
      (f 1))))

(define (exists-word-exists-word-letrec T predicate)
  (lambda (S)
    (let* ([sigma (alphabet T)]
           [s-states (states S)]
           [t-states (states T)]
           [limit (words-up-to-k (length sigma) (* (length s-states) (length t-states)))]
           [gen (wordgenerator sigma)])
      (letrec ([f (lambda (i j w gen2)
                    (let ([wprime (gen2)])
                      (cond [(eq? i limit) null]
                            [(eq? j limit) (f (add1 i) 1 (gen) (wordgenerator sigma))]
                            [(predicate S T w wprime) (list (word w) (word wprime))]
                            [else (f i (add1 j) w gen2)])))])
        (f 1 1 (gen) (wordgenerator sigma))))))

;;;;;;;; stream based methods ;;;;;;;

(define (allwords alphabet)
  (letrec ([f (lambda (currentlist workinglist alphabet)
                (if (empty? workinglist)
                    (let ([newlist (extend-word-by-one alphabet currentlist)])
                          (stream-cons (car newlist) (f newlist (cdr newlist) alphabet)))
                    (stream-cons (car workinglist) (f currentlist (cdr workinglist) alphabet))))])
    (stream-cons '() (f (list null) null alphabet))))

(define (bounded-words alphabet k)
  (letrec ([f (lambda (currentlist workinglist alphabet i k)
                (cond [(> i k) empty-stream]
                      [(and (empty? workinglist) (< i k))
                       (let ([newlist (extend-word-by-one alphabet currentlist)])
                         (stream-cons (car newlist) (f newlist (cdr newlist) alphabet (+ i 1) k)))]
                      [(empty? workinglist) empty-stream]
                      [else (stream-cons (car workinglist) (f currentlist (cdr workinglist) alphabet i k))]))])
    (stream-cons '() (f (list null) null alphabet 0 k))))

(define (print-stream s k)
  (for/list ([i k])
    (printf "~a\n" (stream-ref s i))))

(define (find-counterexample-stream M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)])
  (let ([s (stream-filter (lambda (w) (not (eq? (M1 w) (M2 w)))) (bounded-words alphabet limit))])
    (if (stream-empty? s) "no counterexample" (stream-first s)))))
