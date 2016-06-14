#lang racket

(require racket/list)

(require unstable/automata/machine)
(require unstable/automata/dfa)

(define sigma (list "a" "b"))

(define T
  (dfa s0 (s3)
       [s0 (["a" s0]
            ["b" s1])]
       [s1 (["a" s1]
            ["b" s2])]
       [s2 (["a" s2]
            ["b" s3])]
       [s3 (["a" s3]
            ["b" s3])]))

(define S
  (dfa s0 (s3)
       [s0 (["a" s0]
            ["b" s1])]
       [s1 (["a" s0]
            ["b" s2])]
       [s2 (["a" s0]
            ["b" s3])]
       [s3 (["a" s3]
            ["b" s3])]))

;; sigma size 2 hardcoded for now
(define (convert-base x k) 
  (let ([current-place (expt 2 (- k 1))])
(cond [(= k 0) '()]
      [(>= x current-place) (cons 1 (convert-base (- x current-place) (- k 1)))]
      [#t (cons 0 (convert-base x (- k 1)))])
                                                       ))

(define (get-symbols-list index sigma) 
  (map (lambda (i) (list-ref sigma i)) index))

(define (all-strings k sigma)
  (letrec ([f (lambda (x y) 
                (if (> y k)
                    '()
                    (if (>= (+ x 1) (expt (length sigma) y))
                        (cons (get-symbols-list (convert-base x y) sigma) (lambda() (f 0 (+ y 1))))
                        (cons (get-symbols-list (convert-base x y) sigma) (lambda() (f (+ x 1) y))))))])
    (lambda() (f 0 1))))

(define (union-difference M1 M2 word)
  (let ([onenottwo (and (machine-accepts? M1 word) (not (machine-accepts? M2 word)))]
        [twonotone (and (not (machine-accepts? M1 word)) (machine-accepts? M2 word))])
    (or onenottwo twonotone)))

(define (lang-comp M1 M2 stream func)
  (letrec ([f (lambda (s)
                (let ([pr (s)])
                  (if (null? pr)
                      '()
                      (if (func M1 M2 (car pr))
                          (cons (car pr) (f (cdr pr)))
                          (f (cdr pr))))))])
  ;; special casing epsilon
  (if (func M1 M2 '())
    (cons '() (f stream))
    (f stream))))


;; generate list of all in union of differences, then grab at random/first/last

(define ud (lang-comp T S (all-strings 4 sigma) union-difference))

(printf "Consider the string ~a\n" (first ud))

(printf "Consider the string ~a\n" (last ud))

(printf "Consider the string ~a\n" (list-ref ud (random (length ud))))