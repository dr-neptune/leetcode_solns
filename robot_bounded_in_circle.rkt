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

(define (move-forward curr-dir)
  (match curr-dir
    [#\N (values 0 1)]
    [#\S (values 0 -1)]
    [#\E (values 1 0)]
    [#\W (values -1 0)]))

;; idea
;; Calculate the final vector of how the robot travels after executing
;; all instructions once - it consists of a change in position plus a
;; change in direction.

;; hint
;; The robot stays in the circle if and only if (looking at the final
;; vector) it changes direction (ie. doesn't stay pointing north), or it
;; moves 0.

;; idea
;; calculate end point after instruction set

;; (move-dir)

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

(define (move-forward curr-dir)
  (match curr-dir
    [#\N (values 0 1)]
    [#\S (values 0 -1)]
    [#\E (values 1 0)]
    [#\W (values -1 0)]))

(define (is-robot-bounded instructions)
  (let loop ([ins (string->list instructions)]
             [x 0] [y 0] [dir #\N])
    (if (null? ins)
        (or (and (zero? x) (zero? y))
            (not (equal? dir #\N)))
        (match (first ins)
          [#\G (let-values ([(x+ y+) (move-forward dir)])
                 (loop (rest ins) (+ x x+) (+ y y+) dir))]
          [new-dir (loop (rest ins) x y (turn dir new-dir))]))))

(is-robot-bounded "GGLLGG")
(is-robot-bounded "GG")
(is-robot-bounded "GL")
