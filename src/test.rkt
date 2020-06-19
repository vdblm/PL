#lang racket

(require rackunit)
(require "interp.rkt")

(define (test-path in-path out-path)
  ((test-case
    (check-eq? (value-of-program in-path) (read (open-input-file out-path))))))

(test-path "../samples/test1_in.txt" "../samples/test1_out.txt")