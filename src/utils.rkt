#lang racket
(require "data-types.rkt")

; ndim array
(define (find-elem-by-index lst i)
  (if (zero? i)
      (car lst)
      (find-elem-by-index (cdr lst) (- i 1))))

(define (ndim-array-get lst ind)
  (let ([a (find-elem-by-index lst (car ind))])
    (if (null? (cdr ind))
        a
        (ndim-array-get a (cdr ind)))))

; campare different types
(define (list-elem-cmpr func lst elem)
  (if (null? lst)
      #t
      (and (func (car lst) elem) (list-elem-cmpr func (cdr lst) elem))))

(define (general-less arg1 arg2)
  (cond
    [(number? arg1) (cond
                      [(number? arg2) (< arg1 arg2)]
                      [((listof number?) arg2) (list-elem-cmpr (lambda (a b) (> a b)) arg2 arg1)]
                      [else (error "number cannot be compared!")])]
    [(string? arg1) (cond
                      [(string? arg2) (string<? arg1 arg2)]
                      [((listof string?) arg2) (list-elem-cmpr (lambda (a b) (string>? a b)) arg2 arg1)]
                      [else (error "string cannot be compared!")])]
    [(list? arg1) (cond
                    [(list? arg2) (error "cannot compare two lists")]
                    [else (and (not (general-less arg2 arg1)) (not (general-equal arg2 arg1)))])]
    [else (error "not support this type")]
   ))

(define (general-greater arg1 arg2)
  (general-less arg2 arg1))

(define (general-equal arg1 arg2)
  (cond
    [(number? arg1) (cond
                      [(number? arg2) (equal? arg1 arg2)]
                      [((listof number?) arg2) (list-elem-cmpr (lambda (a b) (equal? a b)) arg2 arg1)]
                      [else #f])]
    [(string? arg1) (cond
                      [(string? arg2) (string=? arg1 arg2)]
                      [((listof string?) arg2) (list-elem-cmpr (lambda (a b) (string=? a b)) arg2 arg1)]
                      [else #f])]
    [(null-exp? arg1) (cond
                        [(null-exp? arg2) #t]
                        [((listof null-exp?) arg2) #t]
                        [else #f])]
    [(boolean? arg1) (cond
                       [(boolean? arg2) (equal? arg1 arg2)]
                       [((listof boolean?) arg2) (list-elem-cmpr (lambda (a b) (equal? a b)) arg2 arg1)]
                       [else #f])]
    [(list? arg1) (cond
                    [(list? arg2) (equal? arg1 arg2)]
                    [else (general-equal arg2 arg1)])]
    [else #f]))

(define (general-unequal arg1 arg2)
  (not (general-equal arg1 arg2)))

; General Arithmetic operations
(define general-add
  (lambda (arg1 arg2)
    (cond
      [(number? arg1) (cond [(number? arg2) (+ arg1 arg2)]
                            [((listof number?) arg2) (cond [(> (length arg2) 1) (append (list (+ arg1 (car arg2))) (general-add arg1 (cdr arg2)))]
                                                           [else (list (general-add arg1 (car arg2)))])]
                            [else (error "Invalid operands for General add function")])]
      [((listof number?) arg1) (cond [(number? arg2) (general-add arg2 arg1)]
                                     [((listof number?) arg2) (append arg1 arg2)]
                                     [else (error "Invalid Operand for General add function")])]
      
      [(boolean? arg1) (cond [(boolean? arg2) (or arg1 arg2)]
                             [((listof boolean?) arg2) (cond [(> (length arg2) 1) (append (list (or arg1 (car arg2))) (general-add arg1 (cdr arg2)))]
                                                             [else (list (general-add arg1 (car arg2)))])]
                             [else (error "General add error")])]
      [((listof boolean?) arg1) (cond [(boolean? arg2) (general-add arg2 arg1)]
                                      [((listof boolean?) arg2) (append arg1 arg2)]
                                      [else (error "General add error")])]
      
      [(string? arg1) (cond [(string? arg2) (string-append arg1 arg2)]
                            [((listof string?) arg2) (cond [(> (length arg2) 1) (append (list (string-append arg1 (car arg2))) (general-add arg1 (cdr arg2)))]
                                                           [else (list (general-add arg1 (car arg2)))])]
                            [else (error "General add error")])]
      [((listof string?) arg1) (cond [(string? arg2) (cond [(> (length arg1) 1) (append (list (string-append (car arg1) arg2)) (general-add (cdr arg1) arg2))]
                                                           [else (list (general-add (car arg1) arg2))])]
                                     [((listof string?) arg2) (append arg1 arg2)]
                                     [else (error "General add error")])]
      [else (error "General add error")])))

(define general-mult
  (lambda (arg1 arg2)
    (cond
      [(number? arg1) (cond [(number? arg2) (* arg1 arg2)]
                            [((listof number?) arg2) (cond [(> (length arg2) 1) (append (list (* arg1 (car arg2))) (general-mult arg1 (cdr arg2)))]
                                                           [else (list (general-mult arg1 (car arg2)))])]
                            [else (error "Invalid operands for General add function")])]
      [((listof number?) arg1) (cond [(number? arg2) (general-mult arg2 arg1)]
                                     [else (error "Invalid Operand for General add function")])]
      
      [(boolean? arg1) (cond [(boolean? arg2) (and arg1 arg2)]
                             [((listof boolean?) arg2) (cond [(> (length arg2) 1) (append (list (and arg1 (car arg2))) (general-mult arg1 (cdr arg2)))]
                                                             [else (list (general-mult arg1 (car arg2)))])]
                             [else (error "General add error")])]
      [((listof boolean?) arg1) (cond [(boolean? arg2) (general-mult arg2 arg1)]
                                      [else (error "General add error")])]
      [else (error "General mult Error")])))

(define general-div
  (lambda (arg1 arg2)
    (cond
      [(number? arg1) (cond [(number? arg2) (/ arg1 arg2)]
                            [((listof number?) arg2) (cond [(> (length arg2) 1) (append (list (/ arg1 (car arg2))) (general-div arg1 (cdr arg2)))]
                                                           [else (list (general-div arg1 (car arg2)))])]
                            [else (error "Invalid operands for General add function")])]
      [((listof number?) arg1) (cond [(number? arg2) (cond [(> (length arg1) 1) (append (list (/ (car arg1) arg2)) (general-div (cdr arg1) arg2))]
                                                           [else (list (general-div (car arg1) arg2))])]
                                     [else (error "Invalid Operand for General add function")])]
      [else (error "General div Error")])))
      
      

(define general-subtract
  (lambda (arg1 arg2)
    (cond
      [(number? arg1) (cond [(number? arg2) (- arg1 arg2)]
                            [((listof number?) arg2) (cond [(> (length arg2) 1) (append (list (- arg1 (car arg2))) (general-subtract arg1 (cdr arg2)))]
                                                           [else (list (general-subtract arg1 (car arg2)))])]
                            [else (error "Invalid operands for General add function")])]
      [((listof number?) arg1) (cond [(number? arg2) (cond [(> (length arg1) 1) (append (list (- (car arg1) arg2)) (general-subtract (cdr arg1) arg2))]
                                                           [else (list (general-subtract (car arg1) arg2))])])]
      [else (error "General Subtract Error")])))

(define general-minus
  (lambda (arg)
    (cond
      [(number? arg) (* arg -1)]
      [((listof number?) arg) (cond [(> (length arg) 1) (append (list (* (car arg) -1)) (general-minus (cdr arg)))]
                                  [else (list (general-minus (car arg)))])]
      [(boolean? arg) (not arg)]
      [((listof boolean?) arg) (cond [(> (length arg) 1) (append (list (not (car arg))) (general-minus (cdr arg)))]
                                  [else (list (general-minus (car arg)))])]
      [else (error "General minus error")])))
    


(provide (all-defined-out))