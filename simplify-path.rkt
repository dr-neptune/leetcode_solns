#lang racket
(require racket)

(require (rename-in racket [simplify-path simple-path]))

(define (simplify-path path)
  (let ([simplified-path (path->string (simple-path path))])
    (match (string-length simplified-path)
      [1 simplified-path]
      [_ (string-trim  simplified-path "/" #:left? #f)])))


(define (simplify-path path)
  (for/fold ([stack '()]
             #:result (if (empty? stack) "/" (foldl (curry string-append "/") "" stack)))
            ([token (string-split path "/")])
    (match token
      [(or "." "") stack]
      [".." (if (empty? stack) stack (rest stack))]
      [_ (cons token stack)])))
