#lang racket
(require racket)

(define exboard
  '((#\5 #\3 #\. #\. #\7 #\. #\. #\. #\.)
    (#\6 #\. #\. #\1 #\9 #\5 #\. #\. #\.)
    (#\. #\9 #\8 #\. #\. #\. #\. #\6 #\.)
    (#\8 #\. #\. #\. #\6 #\. #\. #\. #\3)
    (#\4 #\. #\. #\8 #\. #\3 #\. #\. #\1)
    (#\7 #\. #\. #\. #\2 #\. #\. #\. #\6)
    (#\. #\6 #\. #\. #\. #\. #\2 #\8 #\.)
    (#\. #\. #\. #\4 #\1 #\9 #\. #\. #\5)
    (#\. #\. #\. #\. #\8 #\. #\. #\7 #\9)))


(define exboard2
  '((#\. #\. #\. #\. #\5 #\. #\. #\1 #\.)
    (#\. #\4 #\. #\3 #\. #\. #\. #\. #\.)
    (#\. #\. #\. #\. #\. #\3 #\. #\. #\1)
    (#\8 #\. #\. #\. #\. #\. #\. #\2 #\.)
    (#\. #\. #\2 #\. #\7 #\. #\. #\. #\.)
    (#\. #\1 #\5 #\. #\. #\. #\. #\. #\.)
    (#\. #\. #\. #\. #\. #\2 #\. #\. #\.)
    (#\. #\2 #\. #\9 #\. #\. #\. #\. #\.)
    (#\. #\. #\4 #\. #\. #\. #\. #\. #\.)))

;; helper functions
(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (get-sub-boards sudoku-board)
  "Returns a list of sub-boards in the form '((1 2 3) (4 5 6) (7 8 9))"
  (let ([location-accesors (list first second third)])
    (map (λ (fl)
           (map (λ (f)
                  (map f (fl  (split-into sudoku-board 3))))
                location-accesors))
         location-accesors)))

(define (check-sudoku-dupes ls)
  (if (check-duplicates (filter (λ (v) (not (equal? v #\.))) (flatten ls))) #t #f))

(define board?
  (λ (b) (and (= 3 (length b))
              (char? (caar b)))))

(define (get-sub-board-results board)
  (if (board? board)
      (check-sudoku-dupes board)
      (map get-sub-board-results board)))

(define (check-board-columns board)
  (let* ([location-accesors (list first second third)]
         [board-columns
          (split-into (flatten
                       (map (λ (la)
                              (map (λ (lb)
                                     (map (compose la lb) board))
                                   location-accesors))
                            location-accesors)) 9)])
    (map check-sudoku-dupes board-columns)))

(define (all#t ls)
  (if (zero? (apply + (map (λ (v) (if v 1 0)) ls))) #t #f))

(define (is-valid-sudoku board)
  (let ([board-reshaped (map (λ (r) (split-into r 3)) board)])
    (all#t
     (append
      (map check-sudoku-dupes board-reshaped)
      (check-board-columns board-reshaped)
      (flatten
       (get-sub-board-results (get-sub-boards board-reshaped)))))))
