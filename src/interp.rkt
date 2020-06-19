#lang racket
(require "utils.rkt")
(require "parser.rkt")
(require "data-types.rkt")

;test
(define (value-of-program path)
  (let ([program (open-input-file path)])
  (car(value-of (scan&pars program) '()))))

(define (value-of parser-res env)
  (match parser-res
    ((single-cmd unit-cmd)
     (value-of unit-cmd env))
    
    ((if-unitCmd exp cmd1 cmd2)
     (if (car (value-of exp env))
         (value-of cmd1 env)
         (value-of cmd2 env)))
    
    ((return-unitCmd exp)
     (value-of exp env))
    
    ((greater-exp exp1 exp2)
     (if (> (car (value-of exp1 env)) (car (value-of exp2 env)))
         (list #t env)
         (list #f env)))
    
    ((posNum-exp num)
     (list num env))
    
    ((minus-exp exp)
     (list (-(car(value-of exp env))) env))
    
    ((par-exp exp)
     (value-of exp env))
    
    ((var-exp var)
     (list (apply-env var) env))
    
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
     (list (car (value-of exp env))))
    
    ((multi-lVal exp lVal)
     (cons (car (value-of exp env)) (car (value-of lVal env))))
    
    ((varList-exp var lMem)
     (ndim-array-get (apply-env var) (car (value-of lMem env))))
    
    ((single-lMem exp)
     (list (car (value-of exp env))))
    
    ((multi-lMem exp lMem)
     (cons (car (value-of exp env)) (car (value-of lMem env))))
    )
  )

(value-of-program "../samples/test1.txt")
