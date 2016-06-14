#lang s-exp rosette

(require
  rosette/query/debug rosette/lib/tools/render
  rosette/lib/meta/meta)

(require "automata.rkt")
(require "rosettehintmethods.rkt")

(define S2
  (automaton s0
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s3)
                 (1 → s2)]
             [s3 : (0 → s3)
                 (1 → s3)]))

(define S2_trace
  (automaton3 s0
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
             [s0 : (0 → s1)
                 (1 → s0)]
             [s1 : (0 → s2)
                 (1 → s1)]
             [s2 : accept (0 → s2)
                 (1 → s2)]))

(define (init-vec2d size-i size-j)
  (let ([newvec (make-vector size-j)])
    (for ([sc (in-range size-j)])
      (vector-set! newvec sc (make-vector size-i)))
    newvec))

(define (vec2d-ref v i j)
  (vector-ref (vector-ref v i) j))

(define (vec2d-set! v i j val)
  (vector-set! (vector-ref v i) j val))

; strings should be 1-indexed
(define (diff-index-factor-list l1 l2 i j)
  (define ele-or-null (lambda (lst n)
                        (if (< (length lst) n)
                            null
                            (list-ref lst (sub1 n)))))
  (if (eq? (ele-or-null l1 i) (ele-or-null l2 j))
      0
      1))

(define (min-dist-list v i j l1 l2)
  (min (add1 (vec2d-ref v (sub1 i) j))
       (add1 (vec2d-ref v i (sub1 j)))
       (+ (diff-index-factor-list l1 l2 i j) (vec2d-ref v (sub1 i) (sub1 j)))))

(define (edit-distance-list w wprime)
  (let* ([M (add1 (length w))]
         [N (add1 (length wprime))]
         [vv (init-vec2d M N)])
    (for ([i (in-range M)])
      (vec2d-set! vv 0 i i))
    (for ([j (in-range N)])
      (vec2d-set! vv j 0 j))
    (for ([i (in-range 1 N)])
      (for ([j (in-range 1 M)])
        (vec2d-set! vv i j (min-dist-list vv i j w wprime))))
    (vec2d-ref vv (sub1 N) (sub1 M))))

(define (smallmult-pred M1 M2 w wprime)
  (and (not (same-outcome? M1 M2 w)) (same-outcome? M1 M2 wprime) (>= 2 (edit-distance-list w wprime))))



