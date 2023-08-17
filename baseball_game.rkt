#lang racket
(require racket)

(define (cal-points operations)
  (let loop ([ops operations]
             [record '()])
    (if (null? ops)
        (apply + record)
        (match (first ops)
          ["D" (loop (rest ops) (cons (* 2 (first record)) record))]
          ["C" (loop (rest ops) (drop record 1))]
          ["+" (loop (rest ops) (cons (apply + (take record 2)) record))]
          [v (loop (rest ops) (cons (string->number v) record))]))))

(cal-points '("5" "2" "C" "D" "+"))
(cal-points '("5" "-2" "4" "C" "D" "9" "+" "+"))
(cal-points '("1" "C"))
