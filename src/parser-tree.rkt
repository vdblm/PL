#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/include)
(require "lexer.rkt")
(require "data-types.rkt")


(define zakhm-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar  
                     (command
                      ((unitcommand) (list 'command $1))
                      ((command semicolon unitcommand) (list 'command $1 $3))
                      )
                     (unitcommand
                      ((whilecommand) (list 'whilecom $1))
                      ((ifcommand) (list 'ifcom $1))
                      ((assigncommand) (list 'assign $1))
                      ((returncommand) (list 'return $1))
                      )
                     (whilecommand
                      ((while exp do command end) (list $2 $4))
                      )
                     (ifcommand
                      ((if exp then command else command endif) (list $2 $4 $6))
                      )
                     (assigncommand
                      ((variable assign exp) (list $1 $3))
                      )
                     (returncommand
                      ((return exp) (list $2))
                      )
                     (exp
                      ((aexp) (identity $1))
                      ((aexp greater aexp) (list '> $1 $3))
                      ((aexp less aexp) (list '< $1 $3))
                      ((aexp equal aexp) (list '== $1 $3))
                      ((aexp unequal aexp) (list '!= $1 $3))
                      )
                     (aexp
                      ((bexp) (identity $1))
                      ((bexp minus aexp) (list '- $1 $3))
                      ((bexp plus aexp) (list '+ $1 $3))
                      )
                     (bexp
                      ((cexp) (identity $1))
                      ((cexp mult bexp) (list '* $1 $3))
                      ((cexp div bexp) (list '/ $1 $3))
                      )
                     (cexp
                      ((minus cexp) (list '- $2))
                      ((parO exp parC) (list 'par $2 ))
                      ((posNumber) (list 'posNumber $1))
                      ((null) (identity 'null))
                      ((variable) (list 'variable $1))
                      ((true) (identity 'true))
                      ((false) (identity 'false))
                      ((string) (list 'string $1))
                      ((list) (list 'list $1))
                      ((variable listmem) (list 'listmem $1 $2))
                      )
                     (list
                      ((brackO listvalues brackC) (identity $2 ))
                      ((brackO brackC) (identity 'empty-list))
                      )
                     (listvalues
                      ((exp) (identity $1))
                      ((exp comma listvalues) (list $1 $3))
                      )
                     (listmem
                      ((brackO exp brackC) (identity  $2 ))
                      ((brackO exp brackC listmem) (list $2 $4))
                      )
                     
             )
            )
)
(define my-lexer (lambda(program) (lambda() (dard-lexer program))))
(define (scan&parse program)
  (zakhm-parser (my-lexer program)))

(scan&parse (open-input-file "../tests/test-while-3.txt"))

