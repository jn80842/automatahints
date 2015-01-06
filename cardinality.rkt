#lang racket

(require unstable/automata/machine)
(require unstable/automata/dfa)

(define sigma (list "a" "b"))

;; correct solution
(define T
  (dfa s1 (s1 s2)
       [s1 (["a" s2]
            ["b" s3])]
       [s2 (["a" s3]
            ["b" s1])]
       [s3 (["a" s3]
            ["b" s3])])
)

;; student solution
(define S
  (dfa s1 (s2)
       [s1 (["a" s2]
            ["b" s3])]
       [s2 (["a" s3]
            ["b" s1])]
       [s3 (["a" s3]
            ["b" s3])])


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
  (letrec ([f (lambda (x) (if (> x (expt (length sigma) k))
                              null
                              (cons (get-symbols-list (convert-base x k) sigma) (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))

(define (machine-accepts-length-k M s)
  (letrec ([f (lambda (s count)
     (let ([pr (s)])
       (if (null? pr)
           count
           (if (machine-accepts? M (car pr))
               (f (cdr pr) (+ count 1))
               (f (cdr pr) count)))))])
    (f s 0))
  )

(define (machine-accepts-up-to-length M k)
  (if (machine-accepts? M '())
      (+ 1 (foldl (lambda (i result) (+ result (machine-accepts-length-k M (all-strings i sigma)))) 0 (range 1 (+ k 1))))
      (foldl (lambda (i result) (+ result (machine-accepts-length-k M (all-strings i sigma)))) 0 (range 1 (+ k 1))))
  )

(define (all-strings-count sigma-size n)
  (if [> n 0]
      (+ (expt sigma-size n) (all-strings-count sigma-size (- n 1)))
      1)
  )

(printf "For all strings of length 3 or less (there are ~a), the correct solution accepts ~a and your solution accepts ~a"
        (all-strings-count (length sigma) 3)
        (machine-accepts-up-to-length T 3)
        (machine-accepts-up-to-length S 3))