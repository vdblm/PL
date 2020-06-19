#lang racket

(define (find-elem-by-index lst i)
  (if (zero? i)
      (car lst)
      (find-elem-by-index (cdr lst) (- i 1))))

(define (ndim-array-get lst ind)
  (let ([a (find-elem-by-index lst (car ind))])
    (if (null? (cdr ind))
        a
        (ndim-array-get a (cdr ind)))))

(provide (all-defined-out))