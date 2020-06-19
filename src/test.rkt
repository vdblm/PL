#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "interp.rkt")

(define test-cases
  (test-suite "tests"
              (test-case "if-else"
                         (check-equal? (value-of-program "if 1 > 2 then return [3, 4, 5] else return [4, 3, 45] endif")
                                    '(4 3 45)))
              (test-case "assignment"
                         (check-equal? (value-of-program "")
                                       ))
              (test-case "arithmatic")
              (test-case "list")
              (test-case "n-dim list")
              (test-case "while")
              ; sample tests()


(run-tests test-cases)