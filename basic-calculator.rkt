

Input: s = "(1+(4+5+2)-3)+(6+8)"
Output: (+ (- 3 (+ 1 (+ 4 5 2))) (+ 6 8))




#lang racket

(define (infix-to-prefix expr)
  (define (parse tokens)
    (define (peek) (if (not (empty? tokens)) (first tokens) #f))
    (define (pop) (let ([first-token (peek)]) (set! tokens (rest tokens)) first-token))

    (define (read-expr)
      (define (read-term)
        (cond
          [(number? (peek)) (pop)]
          [(equal? (peek) "(")
           (pop)  ; Skip '('
           (define value (read-expr))
           (when (not (equal? (peek) ")"))
             (error "Mismatched parentheses"))
           (pop)  ; Skip ')'
           value]
          [else (error "Unexpected token" (peek))]))

      (define left (read-term))
      (cond
        [(or (empty? tokens) (equal? (peek) ")")) left]
        [else
         (define op (pop))
         (define right (read-expr))
         (list op left right)]))

    (read-expr))

  (define (expr->prefix expr)
    (if (list? expr)
        (cons (first expr) (map expr->prefix (cdr expr)))
        expr))

  (expr->prefix (parse (tokenize expr))))

(define (tokenize s)
  (define (char->token c)
    (cond
      [(char-numeric? c) (string->number (string c))]
      [(member c '(#\+ #\- #\* #\/ #\( #\))) (string c)]
      [else #f]))
  (filter values (map char->token (string->list s))))

;; Example usage:
(infix-to-prefix "(2 + 3) * 4")


(infix-to-prefix "(1+(4+5+2)-3)+(6+8)")

Input: s = "(1+(4+5+2)-3)+(6+8)"
Output: (+ (- 3 (+ 1 (+ 4 5 2))) (+ 6 8))


'("+" ("+" 1 ("-" ("+" 4 ("+" 5 2)) 3)) ("+" 6 8))



#lang racket

(define (infix-to-prefix expr)
  (define (parse tokens)
    (define (peek) (if (not (empty? tokens)) (first tokens) #f))
    (define (pop) (let ([first-token (peek)]) (set! tokens (rest tokens)) first-token))

    (define (read-expr)
      (define (read-term)
        (cond
          [(number? (peek)) (pop)]
          [(equal? (peek) "(")
           (pop)  ; Skip '('
           (define value (read-expr))
           (when (not (equal? (peek) ")"))
             (error "Mismatched parentheses"))
           (pop)  ; Skip ')'
           value]
          [else (error "Unexpected token" (peek))]))

      (define left (read-term))
      (cond
        [(or (empty? tokens) (equal? (peek) ")")) left]
        [else
         (define op (pop))
         (define right (read-expr))
         (list op left right)]))

    (read-expr))

  (define (expr->prefix expr)
    (if (list? expr)
        (cons (string->symbol (first expr)) (map expr->prefix (cdr expr)))
        expr))

  (expr->prefix (parse (tokenize expr))))

(define (tokenize s)
  (define (char->token c)
    (cond
      [(char-numeric? c) (string->number (string c))]
      [(member c '(#\+ #\- #\* #\/ #\( #\))) (string c)]
      [else #f]))
  (filter values (map char->token (string->list s))))

(define (evaluate-prefix-expr expr)
  (define (evaluate expr)
    (cond
      [(number? expr) expr]
      [(list? expr)
       (define op (car expr))
       (define args (map evaluate (cdr expr)))
       (case op
         [(+) (apply + args)]
         [(-) (apply - args)]
         [(*) (apply * args)]
         [(/) (apply / args)]
         [else (error "Unknown operator" op)])]
      [else (error "Invalid expression" expr)]))
  (evaluate expr))

(define (calculate s)
  (evaluate-prefix-expr (infix-to-prefix s)))

(infix-to-prefix "2-1 + 2")
