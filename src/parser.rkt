#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/include)
(require "lexer.rkt")
(require "data-types.rkt")


(define dard-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar  
                     (command
                      ((unitcommand) (single-cmd $1))
                      ((command semicolon unitcommand) (multi-cmd $1 $3))
                      )
                     (unitcommand
                      ((whilecommand) (identity $1))
                      ((ifcommand) (identity $1))
                      ((assigncommand) (identity $1))
                      ((returncommand) (identity $1))
                      )
                     (whilecommand
                      ((while exp do command end) (while-unitCmd $2 $4))
                      )
                     (ifcommand
                      ((if exp then command else command endif) (if-unitCmd $2 $4 $6))
                      )
                     (assigncommand
                      ((variable assign exp) (assign-unitCmd $1 $3))
                      )
                     (returncommand
                      ((return exp) (return-unitCmd $2))
                      )
                     (exp
                      ((aexp) (identity $1))
                      ((aexp greater aexp) (greater-exp $1 $3))
                      ((aexp less aexp) (less-exp $1 $3))
                      ((aexp equal aexp) (equal-exp $1 $3))
                      ((aexp unequal aexp) (unequal-exp $1 $3))
                      )
                     (aexp
                      ((bexp) (identity $1))
                      ((bexp minus aexp) (subtract-exp $1 $3))
                      ((bexp plus aexp) (plus-exp $1 $3))
                      )
                     (bexp
                      ((cexp) (identity $1))
                      ((cexp mult bexp) (mult-exp $1 $3))
                      ((cexp div bexp) (div-exp $1 $3))
                      )
                     (cexp
                      ((minus cexp) (minus-exp $2))
                      ((parO exp parC) (par-exp $2))
                      ((posNumber) (posNum-exp $1))
                      ((null) (null-exp))
                      ((variable) (var-exp $1))
                      ((true) (bool-exp #t))
                      ((false) (bool-exp #f))
                      ((string) (string-exp $1))
                      ((list) (identity $1))
                      ((variable listmem) (varList-exp $1 $2))
                      )
                     (list
                      ((brackO listvalues brackC) (list-exp $2))
                      ((brackO brackC) (mt-list-exp))
                      )
                     (listvalues
                      ((exp) (single-lVal $1))
                      ((exp comma listvalues) (multi-lVal $1 $3))
                      )
                     (listmem
                      ((brackO exp brackC) (single-lMem $2))
                      ((brackO exp brackC listmem) (multi-lMem $2 $4))
                      )
                     
             )
            )
)
(define my-lexer (lambda(program) (lambda() (dard-lexer program))))
(define (scan&pars program)
  (dard-parser (my-lexer program)))
(provide scan&pars)

