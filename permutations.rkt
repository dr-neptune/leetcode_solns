#lang racket
(require racket)

(define (permute nums)
  (define (backtrack curr)
    (displayln (format "curr: ~a\tanswers: ~a" curr answers))
    (if (equal? (length curr) (length nums))
        (set! answers (cons curr answers))
        (for ([num nums])
          (when (false? (member num curr))
            (begin
              (set! curr (cons num curr))
              (backtrack curr)
              (set! curr (rest curr)))))))
  (define answers '())
  (backtrack '())
  answers)

(permute '(1 2 3))
