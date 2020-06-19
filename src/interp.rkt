#lang racket

(require "parser.rkt")
(require "lexer.rkt")
(require "data_types.rkt")

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this dard-lexer (open-input-string "if 3 > 2 then return 3 else return 2 endif")))
;(my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer)
;(let ((parser-res (dard-parser my-lexer))) parser-res)
(define parser-res (dard-parser my-lexer))

; value-of: expr or command -> (value, (env))
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
    )
  )

(value-of parser-res '())

