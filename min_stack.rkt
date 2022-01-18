#lang racket
(require racket)

(define min-stack%
  (class object%
    (super-new)
    (init-field [stack '()])
    ; push : exact-integer? -> void?
    (define/public (push val)
      (set! stack (append (list val) stack)))
    ; pop : -> void?
    (define/public (pop)
      (set! stack (rest stack)))
    ; top : -> exact-integer?
    (define/public (top)
      (first stack))
    ; get-min : -> exact-integer?
    (define/public (get-min)
      (apply min stack))))


(define obj (new min-stack%))
(send obj push 5)
(send obj push 8)
(send obj push 3)
(send obj pop)
(define param_3 (send obj top))
(define param_4 (send obj get-min))
