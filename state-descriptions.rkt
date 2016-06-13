#lang racket

(require "automata.rkt")
(require "exampledfas.rkt")
(require "bruteforcemethods.rkt")
(require "description.rkt")
(require "descriptionhelpers.rkt")

;;; synthesize descriptions for every state in S3 and T3
;;;
;;; S3 s0: epsilon
;;; S3 s1: 0
;;; S3 s2 (accept): '01' followed by any character
;;; S3 s3: '1' or '00' followed by any character
;;;
;;; T3 s0: a series of 0 or more '1's
;;; T3 s1: any number of '1's followed by any number of '0's
;;; T3 s2 (accept): contains the substring '01'

(define S3-s0
  (automaton s0
             [s0 : (accept : #t)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S3-s1
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #t)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S3-s2
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #t)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #f)
                 (0 → s3)
                 (1 → s3)]))

(define S3-s3
  (automaton s0
             [s0 : (accept : #f)
                 (0 → s1)
                 (1 → s3)]
             [s1 : (accept : #f)
                 (0 → s3)
                 (1 → s2)]
             [s2 : (accept : #f)
                 (0 → s2)
                 (1 → s2)]
             [s3 : (accept : #t)
                 (0 → s3)
                 (1 → s3)]))

(define (check-prefix lang max-length substring)
 (not (for/first ([w (in-producer (words (list 0 1) max-length))]
              #:break (empty? w)
              #:when (not ((language-predicate lang) (word (append substring (word-value w))))))
    #t)))

;; all words with this prefix are members of lang
(define (synthesize-prefix lang max-length)
  (for/first ([prefix (in-producer (words (list 0 1) max-length))]
              #:when (not (for/first ([w (in-producer (words (list 0 1) max-length))]
                                 #:break (empty? w)
                                 #:when (not ((language-predicate lang) (word (append (word-value prefix) (word-value w))))))
                       #t)))
    prefix))

;; all words in lang has this prefix
(define (synthesize-prefix2 lang max-length)
  (for/first ([prefix (in-producer (words (list 0 1) max-length))]
              #:when (not (for/first ([w (in-producer (language-generator lang))]
                                      #:break (empty? w)
                                      #:when (not (is-prefix? prefix w)))
                            #t)))
    prefix))

;; all words in lang has this prefix, and no word NOT in this lang has this prefix
(define (synthesize-prefix3 lang max-length)
  (for/first ([prefix (in-producer (words (list 0 1) max-length))]
              #:when (not (for/first ([w (in-producer (words (list 0 1) max-length))]
                                      #:break (empty? w)
                                      #:when (not (or (and (word-in-lang? w lang) (is-prefix? prefix w))
                                                 (and (not (word-in-lang? w lang)) (not (is-prefix? prefix w))))))
                            #t)))
    prefix))

;; all words in lang has this substring, and not word NOT in this lang has this substring
(define (synthesize-substring lang max-length)
  (for/first ([substring (in-producer (words (list 0 1) max-length))]
              #:when (not (for/first ([w (in-producer (words (list 0 1) max-length))]
                                      #:break (empty? w)
                                      #:when (not (or (and (word-in-lang? w lang) (is-substring? substring w))
                                                 (and (not (word-in-lang? w lang)) (not (is-substring? substring w))))))
                            #t)))
    substring))