#lang racket
(require "utils.rkt")
(require "parser.rkt")
(require "data-types.rkt")
(require "env.rkt")

(define (evaluate path)
  (let ([program (open-input-file path)])
  (car(value-of (scan&pars program) empty-env))))

(define (value-of-program program)
   (car(value-of (scan&pars (open-input-string program)) empty-env)))

(define (value-of parser-res env)
  (match parser-res
    ((single-cmd unit-cmd)
     (value-of unit-cmd env))

    ((multi-cmd cmd unit-cmd)
     (begin
       (define result (value-of cmd env))
       (if (null? (car result))
           (value-of unit-cmd (cadr result))
           (list (car result) env))))

    ((if-unitCmd exp cmd1 cmd2)
     (if (car (value-of exp env))
         (value-of cmd1 env)
         (value-of cmd2 env)))
    
    ((return-unitCmd exp)
     (value-of exp env))

    ((assign-unitCmd var exp)
     (list null (extend-env var (car (value-of exp env)) env)))
; ######### TODOs
    ((assign-func var vars cmd)
     (let ((varsList (car (value-of vars env))))
       (list null (extend-env var (function varsList cmd env) env))))

    ((assign-call var1 var2 args)
     (let ((func (apply-env var2 env))
           (argsVal (car(value-of args env))))
       (match func
         ((function vars cmd saved-env)
          (begin
            (display vars)
          (list null (extend-env var1 (car (value-of cmd (multi-extend-env vars argsVal (extend-env-rec var2 vars cmd saved-env)))) env))))
         (_ (error "wrong function defenition!")))))
        
    ((single-var var)
     (list (list var) env))
    
    ((multi-var var vars)
     (list (cons var (car (value-of vars env))) env))
    
    ((single-arg exp)
     (list (list (car (value-of exp env))) env))
    
    ((multi-arg exp args)
     (list (cons (car (value-of exp env)) (car (value-of args env))) env))

    ((while-unitCmd exp cmd)
     (if (car (value-of exp env))
         (begin
           (if (null? (car (value-of cmd env)))
               (value-of parser-res (cadr (value-of cmd env)))
               (value-of cmd env)))
         (list null env)))

    ((greater-exp exp1 exp2)
     (list (general-greater (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((less-exp exp1 exp2)
     (list (general-less (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((equal-exp exp1 exp2)
     (list (general-equal (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((unequal-exp exp1 exp2)
     (list (general-unequal (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((plus-exp exp1 exp2)
     (list (general-add (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((subtract-exp exp1 exp2)
     (list (general-subtract (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((mult-exp exp1 exp2)
     (list (general-mult (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((div-exp exp1 exp2)
     (list (general-div (car (value-of exp1 env)) (car (value-of exp2 env))) env))

    ((posNum-exp num)
     (list num env))
    
    ((minus-exp exp)
     (list (general-minus (car(value-of exp env))) env))
    
    ((par-exp exp)
     (value-of exp env))
    
    ((var-exp var)
     (list (apply-env var env) env))
    
    ((bool-exp bool-arg)
     (list bool-arg env))
    
    ((string-exp str)
     (list str env))
    
    ((mt-list-exp)
     (list '() env))
    
    ((null-exp)
     (list (null-exp) env))
    
    ((list-exp lVal)
     (value-of lVal env))
    
    ((single-lVal exp)
     (list (list (car (value-of exp env))) env))
    
    ((multi-lVal exp lVal)
     (list (cons (car (value-of exp env)) (car (value-of lVal env))) env))
    
    ((varList-exp var lMem)
      (list (ndim-array-get (apply-env var env) (car (value-of lMem env))) env))
    
    ((single-lMem exp)
     (list (list (car (value-of exp env))) env))
    
    ((multi-lMem exp lMem)
     (list (cons (car (value-of exp env)) (car (value-of lMem env))) env))
    )
  )

(value-of-program "listmaker = func(a, b) {

if a == 0 then return [] else a = listmaker(a-1, b); return a + [b] endif

};

b = listmaker(3, 5);

return b")
(provide value-of-program)

(provide evaluate)

