#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

; ,, ;, while, do, end, if, then, else, endif, variable, return, =, >,
; <, ==, !=, -, +, *, /, (, ), posNumber, null, true, false, string, [,
; ]
(define dard-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posNumber (string->number lexeme)))
            
            ((:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-string (substring lexeme 1 (- (string-length lexeme) 1))))
            ("," (token-comma))
            (";" (token-semicolon))
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
            ("{" (token-cbrackO))
            ("}" (token-cbrackC))
            ("[" (token-brackO))
            ("]" (token-brackC))
            ("endif" (token-endif))
            ("func" (token-funcT))
            ("loadLibrary" (token-loadLib))
            (whitespace (dard-lexer input-port))
            ((eof) (token-EOF))
            ((:+(:or (char-range #\a #\z) (char-range #\A #\Z))) (token-variable lexeme))
            ))

(define-tokens a (posNumber variable string))
(define-empty-tokens b (EOF semicolon plus while do end if then 
                            return assign greater less equal
                            unequal minus mult div parO parC
                            null true false brackO brackC else
                            endif comma funcT cbrackO cbrackC loadLib))

(provide dard-lexer a b)