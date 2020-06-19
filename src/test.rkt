#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "interp.rkt")

(define test-cases
  (test-suite "tests"
              (test-case "1"
                         (check-equal? (value-of-program "if 1 > 2 then return [3, 4, 5] else return [4, 3, 45] endif")
                                    '(4 3 45)))))

(run-tests test-cases)