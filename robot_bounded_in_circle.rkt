#lang racket
(require racket)

(define (turn curr-dir op)
  (if (eq? op #\G)
      curr-dir
      (match (list curr-dir op)
        [(list #\N #\L) #\W]
        [(list #\N #\R) #\E]
        [(list #\S #\L) #\E]
        [(list #\S #\R) #\W]
        [(list #\E #\L) #\N]
        [(list #\E #\R) #\S]
        [(list #\W #\L) #\S]
        [(list #\W #\R) #\N])))

;; idea
;; there should be a nice way to boil this down to simple math
;; failing that, we can heuristically check if after some number
;; of iterations if it returns to the origin n times
