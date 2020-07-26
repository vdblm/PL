#lang racket
(require "data-types.rkt")
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

(define extend-env-rec
  (lambda (var vars cmd saved-env)
    (lambda (search-var)
      (if (equal? search-var var)
          (function vars cmd saved-env)
          (saved-env search-var)))))

; we implicitly assume len(vars) == len(vals)
(define multi-extend-env
  (lambda (vars vals env)
    (lambda (search-var)
      (if (null? vars)
          (env search-var)
          (if (equal? search-var (car vars))
              (car vals)
              ((multi-extend-env (cdr vars) (cdr vals) env) search-var))))))

(define apply-env
  (lambda (var env)
    (env var)))


(provide (all-defined-out))
