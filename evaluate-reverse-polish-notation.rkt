#lang racket
(require racket)

(define (eval-prefix-expr expr)
  (match-let ([(list* op args) expr])
    (match op
      ["+" (apply + args)]
      ["-" (apply - args)]
      ["*" (apply * args)]
      ["/" (apply / args)])))

(define (eval-rpn tokens)
  (for/fold ([stack '()]
             #:result (first stack))
            ([token tokens])
    (match (string->number token)
      [#f (match-let ([(list* num1 num2 rest-stack) stack])
            (cons (truncate (eval-prefix-expr (list token num2 num1))) rest-stack))]
      [n (cons n stack)])))

(eval-rpn '("4" "13" "5" "/" "+"))
(eval-rpn '("10" "6" "9" "3" "+" "-11" "*" "/" "*" "17" "+" "5" "+"))
