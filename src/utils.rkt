#lang racket
(require "data-types.rkt")

; ndim array
(define (find-elem-by-index lst i)
  (if (zero? i)
      (car lst)
      (find-elem-by-index (cdr lst) (- i 1))))

(define (ndim-array-get lst ind)
  (let ([a (find-elem-by-index lst (car ind))])
    (if (null? (cdr ind))
        a
        (ndim-array-get a (cdr ind)))))

; campare different types
(define (list-elem-cmpr func lst elem)
  (if (null? lst)
      #t
      (and (func (car lst) elem) (list-elem-cmpr func (cdr lst) elem))))

(define (general-less arg1 arg2)
  (cond
    [(number? arg1) (cond
                      [(number? arg2) (< arg1 arg2)]
                      [((listof number?) arg2) (list-elem-cmpr (lambda (a b) (> a b)) arg2 arg1)]
                      [else (error "number cannot be compared!")])]
    [(string? arg1) (cond
                      [(string? arg2) (string<? arg1 arg2)]
                      [((listof string?) arg2) (list-elem-cmpr (lambda (a b) (string>? a b)) arg2 arg1)]
                      [else (error "string cannot be compared!")])]
    [(list? arg1) (cond
                    [(list? arg2) (error "cannot compare two lists")]
                    [else (and (not (general-less arg2 arg1)) (not (general-equal arg2 arg1)))])]
    [else (error "not support this type")]
   ))

(define (general-greater arg1 arg2)
  (general-less arg2 arg1))

(define (general-equal arg1 arg2)
  (cond
    [(number? arg1) (cond
                      [(number? arg2) (equal? arg1 arg2)]
                      [((listof number?) arg2) (list-elem-cmpr (lambda (a b) (equal? a b)) arg2 arg1)]
                      [else #f])]
    [(string? arg1) (cond
                      [(string? arg2) (string=? arg1 arg2)]
                      [((listof string?) arg2) (list-elem-cmpr (lambda (a b) (string=? a b)) arg2 arg1)]
                      [else #f])]
    [(null-exp? arg1) (cond
                        [(null-exp? arg2) #t]
                        [((listof null-exp?) arg2) #t]
                        [else #f])]
    [(boolean? arg1) (cond
                       [(boolean? arg2) (equal? arg1 arg2)]
                       [((listof boolean?) arg2) (list-elem-cmpr (lambda (a b) (equal? a b)) arg2 arg1)]
                       [else #f])]
    [(list? arg1) (cond
                    [(list? arg2) (equal? arg1 arg2)]
                    [else (general-equal arg2 arg1)])]
    [else #f]))

(define (general-unequal arg1 arg2)
  (not (general-equal arg1 arg2)))

(provide (all-defined-out))