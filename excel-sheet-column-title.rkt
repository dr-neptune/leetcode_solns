#lang racket
(require racket)

(define (convert-to-title columnNumber [vals '()])
  (if (<= columnNumber 0)
      (list->string vals)
      (convert-to-title (quotient (sub1 columnNumber) 26)
                        (append (list (integer->char (+ (remainder (sub1 columnNumber) 26) 65)))
                                vals))))

(get-thumbprint 2)
(get-thumbprint 26)
(get-thumbprint 27)
(get-thumbprint 703)
