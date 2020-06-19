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

;test
;(define env (empty-env))
;(define env1 (extend-env 'a 2 env))
;(define env2 (extend-env 'b 3 env1))
;(define env3 (extend-env 'a 5 env2))
;(apply-env 'a env1)

(provide (all-defined-out))
