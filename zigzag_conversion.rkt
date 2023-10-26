#lang racketcx
(require racket)

#|

The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)

P   A   H   N
A P L S I I G
Y   I   R

And then read line by line: "PAHNAPLSIIGYIR"

Write the code that will take a string and make this conversion given a number of rows:

string convert(string s, int numRows);



Example 1:

Input: s = "PAYPALISHIRING", numRows = 3
Output: "PAHNAPLSIIGYIR"

Example 2:

Input: s = "PAYPALISHIRING", numRows = 4
Output: "PINALSIGYAHRPI"
Explanation:
P     I    N
A   L S  I G
Y A   H R
P     I

Example 3:

Input: s = "A", numRows = 1
Output: "A"

|#


(define exstr "PAYPALISHIRING")
(define exrows 3)



#|

idea

make a function that takes a length and divisor and creates a list that goes up and down
like 1 2 3 2 1 2 3 2 1 2 3 2 1 ... etc for the length of the input

|#

(let ([num-vals 14]
      [num-rows 3])
   (let loop ([curr-iter 1]
              [op add1]
              [iters-left num-vals])
    (displayln curr-iter)
    (if (zero? iters-left)
        'done
        (match curr-iter
          [(== (sub1 num-rows)) (loop (op curr-iter) add1 (sub1 iters-left))]
          [(== num-rows) (loop (sub1 curr-iter) sub1 (sub1 iters-left))]
          [_ (loop (op curr-iter) op (sub1 iters-left))]))))

(define exstr "A")

(let ([num-vals (string-length exstr)]
      [num-rows 1])
  (list->string
   (flatten
   (map
   (λ (group)
     (map cdr group))
   (group-by
   (λ (pair) (modulo (car pair) num-rows))
  (map cons
   (let loop ([curr-iter 1]
              [op add1]
              [iters-left num-vals])
     (displayln curr-iter)
     (if (zero? iters-left)
         '()
         (match curr-iter
           [2 (cons curr-iter (loop (op curr-iter) add1 (sub1 iters-left)))]
           [(== num-rows) (cons curr-iter (loop (sub1 curr-iter) sub1 (sub1 iters-left)))]
           [_ (cons curr-iter (loop (op curr-iter) op (sub1 iters-left)))])))
   (string->list exstr)))))))

;; not working for n = 4


(define (convert s numRows)
  (let ([num-vals (string-length s)])
    (list->string
     (flatten
      (map
       (λ (group)
         (map cdr group))
       (group-by
        (λ (pair) (modulo (car pair) numRows))
        (map cons
             (let loop ([curr-iter 1]
                        [op add1]
                        [iters-left num-vals])
               (if (zero? iters-left)
                   '()
                   (match curr-iter
                     [2 (cons curr-iter (loop (op curr-iter) add1 (sub1 iters-left)))]
                     [(== numRows) (cons curr-iter (loop (sub1 curr-iter) sub1 (sub1 iters-left)))]
                     [_ (cons curr-iter (loop (op curr-iter) op (sub1 iters-left)))])))
             (string->list s))))))))


(define (convert s numRows)
  (let ([num-vals (string-length s)]
        [strls (string->list s)])
    ((compose list->string
              flatten
              (curry map (λ (group) (map cdr group)))
              (curry group-by (λ (pair) (modulo (car pair) numRows)))
              (curry map cons))
     (let loop ([curr-iter 1]
                [op add1]
                [iters-left num-vals])
       (if (zero? iters-left)
           '()
           (match curr-iter
             [2 (cons curr-iter (loop (op curr-iter) add1 (sub1 iters-left)))]
             [(== numRows) (cons curr-iter (loop (sub1 curr-iter) sub1 (sub1 iters-left)))]
             [_ (cons curr-iter (loop (op curr-iter) op (sub1 iters-left)))])))
     strls)))

(convert exstr 4)
