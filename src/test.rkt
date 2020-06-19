#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "interp.rkt")

(define test-cases
  (test-suite "tests"
              (test-case "if-else"
                         (check-equal? (evaluate "../tests/test-if-else.txt")
                                    '(4 3 45)))
              (test-case "assignment"
                         (check-equal? (evaluate "../tests/test-assignment.txt")
                                       2))
;              (test-case "arithmatic")
               (test-case "list"
                          (check-equal? (evaluate "../tests/test-list.txt")
                                       2))
;              (test-case "while")
               (test-case "sample0"
                          (check-equal? (evaluate "../tests/test-sample0.txt")
                                       '(2 2)))               
               (test-case "sample1"
                          (check-equal? (evaluate "../tests/test-sample1.txt")
                                       5))
               (test-case "sample2"
                          (check-equal? (evaluate "../tests/test-sample2.txt")
                                       #t))
               (test-case "sample3"
                          (check-equal? (evaluate "../tests/test-sample3.txt")
                                       '(7 7/2 7/3 7/5 1 55)))
               (test-case "sample4"
                          (check-equal? (evaluate "../tests/test-sample4.txt")
                                       35))
              ))


(run-tests test-cases)