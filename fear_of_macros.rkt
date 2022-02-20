#lang racket
(require racket)

;; 3.1: What is a syntax transformer?

(define-syntax foo
  (lambda (stx)
    (syntax "I am foo")))

(foo)

(define-syntax (also-foo stx)
  (syntax "I am also foo"))

(also-foo)

(define-syntax (quoted-foo stx)
  #'"I am also foo, using #' instead of syntax")

(quoted-foo)

(define-syntax (say-hi stx)
  #'(displayln "hi"))

(say-hi)

;; 3.2: What's the input?
(define-syntax (show-me stx)
  (print stx)
  #'(void))

(show-me '(+ 1 2))


;; define a piece of syntax
(define stx #'(if x (list "true") #f))

;; use functions that access the syntax object
(syntax-source stx)
(syntax-line stx)
(syntax-column stx)

;; convert completely to an s-expr
(syntax->datum stx)

;; return a list that has syntax objects
(syntax-e stx)

;; in most cases, syntax->list gives the same results as syntax-e
(syntax->list stx)

;; 3.3: Actually Transforming the Input
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(datum->syntax #'(a b c d) (reverse (cdr(syntax->datum #'(a b c d)))))

(reverse-me "backwards" "am" "i" values)

(datum->syntax #f (reverse (cdr (syntax->datum #'(reverse-me "backwards" "am" "i" values)))))

(values "i" "am" "backwards")

;; 3.4: Compile Time vs. Run Time
(define-syntax (foo stx)
  (make-pipe)
  #'(void))

;; if we implement if as a function, all of the arguments would be evaluated before being provided to the function
(define (our-if condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

;; seems to work
(our-if #t "true" "false")

;; caveat
(define (display-and-return x)
  (displayln x)
  x)

;; since args are eagerly evaluated, we run both side-effect causing functions which is problematic
(our-if #t (display-and-return "true") (display-and-return "false"))

;; instead we can rearrange our code
(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))

(our-if-v2 #t (display-and-return "true") (display-and-return "false"))
(our-if-v2 #f (display-and-return "true") (display-and-return "false"))

(syntax->list #'(our-if-v2 #t (display-and-return "true") (display-and-return "false")))

;; make the syntax object
(define stx #'(our-if-v2 #t "true" "false"))
(displayln stx)

;; change the syntax object into a list of syntax objects
(define xs (syntax->list stx))
(displayln xs)

;; change it into a racket cond form
`(cond [,(cadr xs) ,(caddr xs)]
       [else ,(cadddr xs)])

;; change it into syntax
(datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                          [else ,(cadddr xs)]))

;; alternatively, using match instead of c***r
(define-syntax (our-if-using-match stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

(our-if-using-match #t "true" "false")

;; since macros are expanded at compile-time, they only have access to racket-base
;; we need to require match for compile time
(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

(our-if-using-match-v2 #t "true" "false")

;; 3.5: begin-for-syntax
;; suppose we want to use a helper function to be used by a macro
;; we can't just define it since it won't exist at compile time
;; we can do this by putting the definition of the helper function(s) inside begin-for-syntax
(begin-for-syntax
  (define (my-helper-function ....)
    ....))

(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)

;; in the simple case, we can also use define-for-syntax, which composes begin-for-syntax and define
(define-for-syntax (my-helper-function ....)
  ....)

(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)


;; review of chapter 3

;; syntax transformers work at compile time, not run time
;; this allows us to rearrange the pieces of syntax without evaluating them
;; we can also implement functions like if which couldn't work as a run time function

;; syntax-transformers use basic racket expressions
;; but we need to be cognizant that they are not working at run-time, but compile time

;; only racket/base is required for us automatically
;; if we need other modules, we have to require them *for compile time*
;; if we want to define helper functions, we need to wrap the defs in a begin-for-syntax or define-for-syntax form to make them available at compile time

;; Chapter 4
