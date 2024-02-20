#lang racket
(require racket)

(define-match-expander win-states
  (λ (stx)
    (syntax-case stx ()
      [(_ p)
       #'(or (vector (vector p p p)
                     (vector _ _ _)
                     (vector _ _ _))
             (vector (vector _ _ _)
                     (vector p p p)
                     (vector _ _ _))
             (vector (vector _ _ _)
                     (vector _ _ _)
                     (vector p p p))
             (vector (vector p _ _)
                     (vector p _ _)
                     (vector p _ _))
             (vector (vector _ p _)
                     (vector _ p _)
                     (vector _ p _))
             (vector (vector _ _ p)
                     (vector _ _ p)
                     (vector _ _ p))
             (vector (vector p _ _)
                     (vector _ p _)
                     (vector _ _ p))
             (vector (vector _ _ p)
                     (vector _ p _)
                     (vector p _ _)))])))

(define (make-grid moves)
  (define (update-grid! grid x y value)
    (let ((row (vector-ref grid y)))
      (vector-set! row x value))
    grid)
  (let ([grid (vector (vector 0 0 0)
                      (vector 0 0 0)
                      (vector 0 0 0))])
    (for ([move moves]
          [player (in-cycle (list 'X 'O))])
      (match-let ([(list y x) move])
        (update-grid! grid x y player)))
    grid))

(define (tictactoe moves)
  (match (make-grid moves)
    [(win-states 'X) "A"]
    [(win-states 'O) "B"]
    [_ #:when (< (length moves) 9) "Pending"]
    [_ "Draw"]))


(define exmoves '((0 0) (1 1) (2 0) (1 0) (1 2) (2 1) (0 1) (0 2) (2 2)))
(define exmoves2 '((0 0) (1 1) (0 1) (0 2) (1 0) (2 0)))
(define exmoves3 '((0 0) (2 0) (1 1) (2 1) (2 2)))
(tictactoe exmoves3)
