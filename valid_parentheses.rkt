#lang racket
(require racket)

;; idea
;; add all the members of the string to a stack one by one
;; if the item added in is the ending tag for the previous item, remove both
;; if there are no items left in the stack after, then it is valid


(define *delimiters*
  (hash
   '#\) #\(
   '#\} #\{
   '#\] #\[
   ))


(define (is-valid str)
  (define (stack-matcher strls [stack '()])
    (if (not (empty? strls))
        (let ([end-delim (hash-ref *delimiters* (first strls) #f)])
          (cond [(not end-delim) (stack-matcher (rest strls) (append stack (list (first strls))))]
                [(and end-delim (empty? stack)) #f]
                [(equal? (last stack) end-delim)
                 (stack-matcher (rest strls) (drop-right stack 1))]
                [else #f]))
        (empty? stack)))
  (stack-matcher (string->list str)))


(module+ test
  (require rackunit)
  (check-false (is-valid ")"))
  (check-true (is-valid "()"))
  (check-true (is-valid "()[]{}"))
  (check-false (is-valid "(]")))
