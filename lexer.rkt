#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

; ;, while, do, end, if, then, else, endif, variable, return, =, >,
; <, ==, !=, -, +, *, /, (, ), posNumber, null, true, false, string, [,
; ]
(define dard-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posNumber (string->number lexeme)))
            ((:+(:or (char-range #\a #\z) (char-range #\A #\Z))) (token-variable lexeme))
            ;((:: any-string) (token-string lexeme))
            ("while" (token-while))
            ("do" (token-do))
            ("end" (token-end))
            ("if" (token-if))
            ("then" (token-then))
            ("else" (token-else))
            ("endif" (token-endif))
            ("return" (token-return))
            ("=" (token-assign))
            (">" (token-greater))
            ("<" (token-less))
            ("==" (token-equal))
            ("!=" (token-unequal))
            ("-" (token-minus))
            ("+" (token-plus))
            ("*" (token-mult))
            ("/" (token-div))
            ("(" (token-parO))
            (")" (token-parC))
            ("null" (token-null))
            ("true" (token-true))
            ("false" (token-false))
            ("[" (token-brackO))
            ("]" (token-brackC))
            ("endif" (token-endif))
            (whitespace (dard-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (posNumber variable string))
(define-empty-tokens b (EOF plus while do end if then else endif
                            return assign greater less equal
                            unequal minus mult div parO parC
                            null true false brackO brackC))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this dard-lexer (open-input-string "a = 1+2+ 3 +   4")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
