#lang eopl

(define-datatype command command?
    (single-cmd 
     (unit-cmd unitCommand?))
    (multi-cmd
     (cmd command?)
     (unit-cmd unitCommand?)))

(define-datatype unitCommand unitCommand?
    (while-unitCmd
      (exp expression?)
      (cmd command?))
    (if-unitCmd
      (exp expression?)
      (cmd1 command?)
      (cmd2 command?))
    (assign-unitCmd
      (var symbol?)
      (exp expression?))
    (return-unitCmd
      (exp expression?)))

(define-datatype expression expression?
    (greater-exp
    (exp1 expression?)
    (exp2 expression?))
    (less-exp
    (exp1 expression?)
    (exp2 expression?))
    (equal-exp
    (exp1 expression?)
    (exp2 expression?))
    (unequal-exp
    (exp1 expression?)
    (exp2 expression?))
    (subtract-exp
    (exp1 expression?)
    (exp2 expression?))
    (plus-exp
    (exp1 expression?)
    (exp2 expression?))
    (mult-exp
    (exp1 expression?)
    (exp2 expression?))
    (div-exp
    (exp1 expression?)
    (exp2 expression?))
    (minus-exp
    (exp expression?))
    (par-exp
    (exp expression?))
    (posNum-exp
    (num number?))
    (null-exp)
    (var-exp
    (var symbol?))
    (true-exp)
    (false-exp)
    (string-exp
    (str string?))
    (empty-list-exp)
    (list-exp
    (lVal listValues?))
    (varList-exp
    (var symbol?)
    (lMem listMem?)))

(define-datatype listValues listValues?
    (single-lVal
    (exp expression?))
    (mult-lVal
    (exp expression?)
    (lVal listValues?)))

(define-datatype listMem listMem?
    (single-lMem
    (exp expression?))
    (mult-lMem
    (exp expression?)
    (lMem listMem?)))

