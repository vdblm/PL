#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(define dard-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posNumber (string->number lexeme)))
            
            ((:: "\"" (:* any-char) "\"") (token-string (substring lexeme 1 (- (string-length lexeme) 1))))
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
            ("[" (token-brackO))
            ("]" (token-brackC))
            ("endif" (token-endif))
            (whitespace (dard-lexer input-port))
            ((eof) (token-EOF))
            ((:+(:or (char-range #\a #\z) (char-range #\A #\Z))) (token-variable lexeme))))

(define-tokens a (posNumber variable string))
(define-empty-tokens b (EOF semicolon plus while do end if then 
                            return assign greater less equal
                            unequal minus mult div parO parC
                            null true false brackO brackC else
                            endif comma))

(define dard-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar  
                     (command
                      ((unitcommand) (list 'unitcommand $1))
                      ((command semicolon unitcommand) (list 'multicommand $1 $3))
                      )
                     (unitcommand
                      ((whilecommand) (list 'whilecommand $1))
                      ;((ifcommand) (list 'ifcommand $1))
                      ((assigncommand) (list 'assigncommand $1))
                      ;((return) (list 'return $1))
                      )
                     (whilecommand
                      ((while exp do command end) (list $2 $4))
                      )
                     (assigncommand
                      ((variable assign exp) (list $1 $3))
                      )
                     (exp
                      ((aexp) (list 'aexp $1))
                      ((aexp greater aexp) (list 'greater $1 $3))
                      ((aexp less aexp) (list 'less $1 $3))
                      ((aexp equal aexp) (list 'equal $1 $3))
                      ((aexp unequal aexp) (list 'unequal $1 $3))
                      )
                     (aexp
                      ((bexp) (list $1))
                      ((bexp minus aexp) (list 'minus $1 $3))
                      ((bexp plus aexp) (list 'plus $1 $3))
                      )
                     (bexp
                      ((cexp) (list $1))
                      ((cexp mult bexp) (list 'mult $1 $3))
                      ((cexp div bexp) (list 'div $1 $3))
                      )
                     (cexp
                      ((minus cexp) (list 'sign $2))
                      ((parO exp parC) (list 'par $2))
                      ((posNumber) (list $1))
                      ((null) (list 'null))
                      ((variable) (list $1))
                      ((true) (list 'true))
                      ((false) (list 'false))
                      ((string) (list $1))
                      ((list) (list 'list $1))
                      ((variable listmem) (list 'index $1 $2))
                      )
                     (list
                      ((brackO listvalues brackC) (list $2))
                      ((brackO brackC) (list 'null))
                      )
                     (listvalues
                      ((exp) (list $1))
                      ((exp comma listvalues) (list $1 $3))
                      )
                     (listmem
                      ((brackO exp brackC) (list $2))
                      ((brackO exp brackC listmem) (list $2 $4))
                      )
                     
             )
            )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this dard-lexer (open-input-string "while a == 2 do b = -3 end;")))
;(my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer) (my-lexer)
(let ((parser-res (dard-parser my-lexer))) parser-res)

