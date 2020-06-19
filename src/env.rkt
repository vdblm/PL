#lang racket

(define empty-env
  (lambda ()
    (lambda (var)
      (error var "not found!"))))
    

(define extend-env
  (lambda (var value env)
    ;(display var value)
    (lambda (search-var)
      (if (equal? search-var var)
          value
          (env search-var)))))


(define apply-env
  (lambda (var env)
    (env var)))


(provide (all-defined-out))
