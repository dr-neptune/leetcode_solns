#lang racket
(require math/matrix)

(define (multiply mat1 mat2)
  (apply (compose matrix->list* matrix*)
         (map list*->matrix (list mat1 mat2))))
