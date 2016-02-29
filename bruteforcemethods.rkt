#lang racket

(require racket/generator)
(require "automata.rkt")

(provide wordgenerator words find-counterexample find-failing-prefix exists-word exists-word-f
         exists-word-forall-words exists-word-forall-words-f
         forall-words
         exists-word-exists-word exists-word-exists-word-f)
(define sigma2 (list "00" "01" "10" "11"))
(define firstlist (list (list "00") (list "01" ) (list "10") (list "11")))

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

(define (exists-word-f gen T predicate)
  (lambda (S)
    (for/first ([w (in-producer gen)]
                #:when (or (empty? w) (predicate S T (word-value w))))
      w)))

; (predicate M1 M2 word) returns #t if word should be returned, else #f
(define (exists-word M1 M2 predicate)
  (let* ([sigma (alphabet M2)]
         [m1states (states M1)]
         [m2states (states M2)]
         [limit (words-up-to-k (length sigma) (* (length m1states) (length m2states)))]
         [gen (wordgenerator sigma)])
    (for/first ([i (in-range (add1 limit))]
                [w (in-producer gen)]
                #:when (or (predicate M1 M2 w) (eq? i limit)))
      (if (eq? i limit)
      null
      (word w))
      )))

; test if some predicate is true for all words wrt some word w
(define (forall-words M1 M2 predicate w)
  (let* ([sigma (alphabet M2)]
         [m1states (states M1)]
         [m2states (states M2)]
         [limit (words-up-to-k (length sigma) (* (length m1states) (length m2states)))]
         [gen (wordgenerator sigma)])
    (for/first ([i (in-range (add1 limit))]
                [wprime (in-producer gen)]
                #:when (or (eq? i limit) (not (predicate M1 M2 w wprime))))
      (eq? i limit) ; return true if we checked all wprime makes predicate true, false otherwise
      )))

(define (exists-word-forall-words M1 M2 predicate)
  (let* ([sigma (alphabet M2)]
         [m1states (states M1)]
         [m2states (states M2)]
         [limit (words-up-to-k (length sigma) (* (length m1states) (length m2states)))]
         [gen (wordgenerator sigma)]
         [result (for/first ([i (in-range (add1 limit))]
                            [w (in-producer gen)]
                            #:when (forall-words M1 M2 predicate w))
                  (word w))])
    (if (word? result)
        result
        null)))

(define (exists-word-forall-words-f T predicate)
  (lambda (S)
    (let* ([sigma (alphabet T)]
         [s-states (states S)]
         [t-states (states T)]
         [limit (words-up-to-k (length sigma) (* (length s-states) (length t-states)))]
         [gen (wordgenerator sigma)]
         [result (for/first ([i (in-range (add1 limit))]
                            [w (in-producer gen)]
                            #:when (forall-words S T predicate w))
                  (word w))])
    (if (word? result)
        result
        null))))

(define (same-final-state? M w wprime)
  (eq? (M w) (M wprime)))
(define (diff-outcomes? M w wprime)
  (not (eq? (M w) (M wprime))))

(define (exists-word-exists-word M1 M2 predicate)
  (let* ([sigma (alphabet M2)]
         [m1states (states M1)]
         [m2states (states M2)]
        [limit (words-up-to-k (length sigma) (* (length m1states) (length m2states)))]
        [gen (wordgenerator sigma)])
    (letrec ([f (lambda (i j w gen2)
                (let ([wprime (gen2)])
                  (cond [(eq? i limit) null]
                        [(eq? j limit) (f (add1 i) 1 (gen) (wordgenerator sigma))]
                        [(predicate M1 M2 w wprime) (list (word w) (word wprime))]
                        [else (f i (add1 j) w gen2)])))])
      (f 1 1 (gen) (wordgenerator sigma)))))

(define (exists-word-exists-word-f T predicate)
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

;; stream version isn't working
(define (find-failing-prefix-stream M1 M2 alphabet k)
  (let ([s (stream-filter (lambda (p) (stream-andmap
                                            (lambda (w) (not (same-outcome? M1 M2 (append p w)))) (bounded-words alphabet k)))
                               (bounded-words alphabet k))])
    (if (stream-empty? s) "no such prefix" (stream-first s))))

(define (find-split-state-stream M1 M2 alphabet k)
  (let ([s (allwords alphabet)])
    (letrec ([f (lambda (M1 M2 i j k)
                  (cond [(and (same-final-state? M1 (stream-ref s i) (stream-ref s j))
                              (not (same-outcome? M2 (stream-ref s i) (stream-ref s j)))) (list (M1 (stream-ref s i)) (stream-ref s i) (stream-ref s j))]
                        [(eq? i k) "no split state"]
                        [(eq? j k) (f M1 M2 (+ i 1) 0 k)]
                        [else (f M1 M2 i (+ j 1) k)]))])
      (f M1 M2 0 0 k))))

;;;;;;; old non refactored methods ;;;;;;;;;

(define (find-counterexample M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) "no counterexample"]
                          [(not (same-outcome? M1 M2 w)) w]
                          [else (f (add1 i))])))])
      (f 1))))

(define (failing-prefix M1 M2 alphabet k p)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (cond [(eq? i limit) #t]
                        [(same-outcome? M1 M2 (append p (gen))) #f]
                        [else (f (add1 i))]))])
      (f 1))))

(define (find-failing-prefix M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                (let ([p (gen)])
                  (cond [(eq? i limit) "no such prefix"]
                        [(failing-prefix M1 M2 alphabet k p) p]
                        [else (f (add1 i))])))])
      (f 1))))

(define (find-split-state M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i j w gen2)
                  (let ([wprime (gen2)])
                    (cond [(eq? i limit) "no split state"]
                          [(eq? j limit) (f (add1 i) 1 (gen) (wordgenerator alphabet))]
                          [(and (same-final-state? M1 w wprime) (diff-outcomes? M2 w wprime)) (list (M1 w) w wprime)]
                          [else (f i (add1 j) w gen2)])))])
      (f 1 1 (gen) (wordgenerator alphabet)))))