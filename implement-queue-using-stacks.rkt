#lang racket
(require racket)

(define my-queue%
  (class object%
    (super-new)
    (init-field [stack '()])
    ; push : exact-integer? -> void?
    (define/public (push x)
      (set! stack (cons x stack)))
    ; pop : -> exact-integer?
    (define/public (pop)
      (begin
        (let ([first-element (last stack)])
          (set! stack (drop-right stack 1))
          first-element)))
    ; peek : -> exact-integer?
    (define/public (peek)
      (last stack))
    ; empty : -> boolean?
    (define/public (empty)
      (if (empty? stack) #t #f))))
