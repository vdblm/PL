#lang racket
(require "utils.rkt")
(require "parser.rkt")
(require "data-types.rkt")
(require "env.rkt")

(define (evaluate path)
  (let ([program (open-input-file path)])
  (car(value-of (scan&pars program) '()))))

(define (value-of-program program)
   (car(value-of (scan&pars (open-input-string program)) '())))

(define (value-of parser-res env)
  (match parser-res
    ((single-cmd unit-cmd)
     (value-of unit-cmd env))

    ((multi-cmd command unitcommand)
     (begin
       (define result (value-of command env))
       (if (null? (car result))
           (value-of unitcommand (cdr result))
           (list (car result) env))))

    ((if-unitCmd exp cmd1 cmd2)
     (if (car (value-of exp env))
         (value-of cmd1 env)
         (value-of cmd2 env)))
    
    ((return-unitCmd exp)
     (value-of exp env))

    ((assign-unitCmd var exp)
     (list null (extend-env var (car (value-of exp env)) env)))

    ((while-unitCmd exp command)
     (if (car (value-of exp env))
         (value-of command env)
         (list 'EndWhile env)))  ; ???

    ((greater-exp exp1 exp2)
     (if (> (car (value-of exp1 env)) (car (value-of exp2 env)))
         (list #t env)
         (list #f env)))

    ((less-exp exp1 exp2)
     (if (< (car (value-of exp1 env)) (car (value-of exp2 env)))
         (list #t env)
         (list #f env)))

    ((equal-exp exp1 exp2)
     (if (eqv? (car (value-of exp1 env)) (car (value-of exp2 env)))
         (list #t env)
         (list #f env)))

    ((unequal-exp exp1 exp2)
     (if (eqv? (car (value-of exp1 env)) (car (value-of exp2 env)))
         (list #f env)
         (list #t env)))

    ((plus-exp exp1 exp2)
     (list (+ (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((subtract-exp exp1 exp2)
     (list (- (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((mult-exp exp1 exp2)
     (list (* (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((div-exp exp1 exp2)
     (list (/ (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((posNum-exp num)
     (list num env))
    
    ((minus-exp exp)
     (list (-(car(value-of exp env))) env))
    
    ((par-exp exp)
     (value-of exp env))
    
    ;((var-exp var)
     ;(list (apply-env var) env))
    
    ((bool-exp bool-arg)
     (list bool-arg env))
    
    ((string-exp str)
     (list str env))
    
    ((mt-list-exp)
     (list '() env))
    
    ((null-exp)
     (list "null" env))
    
    ((list-exp lVal)
     (value-of lVal env))
    
    ((single-lVal exp)
     (list (list (car (value-of exp env))) env))
    
    ((multi-lVal exp lVal)
     (list (cons (car (value-of exp env)) (car (value-of lVal env))) env))
    
    ;((varList-exp var lMem)
     ;list((ndim-array-get (apply-env var) (car (value-of lMem env))) env))
    
    ((single-lMem exp)
     (list (list (car (value-of exp env)) env)))
    
    ((multi-lMem exp lMem)
     (cons (car (value-of exp env)) (car (value-of lMem env))))


     (list (cons (car (value-of exp env)) (car (value-of lMem env))) env))
    )
  )

(provide value-of-program)

