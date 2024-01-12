#lang racket
(require racket)

(define (calculate-time keyboard word)
  (match-let* ([(list keeb-ls word-ls) (map string->list (list keyboard word))]
               [indices (cons 0 (map (curry index-of keeb-ls) word-ls))])
    (for/sum ([idx indices]
              [ridx (rest indices)])
      (abs (- ridx idx)))))
