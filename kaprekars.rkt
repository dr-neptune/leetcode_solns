#lang racket
(require racket
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut)
         plot)

(plot-new-window? #t)

;; calculate the number of iterations it takes to get to Kaprekar's Constant
(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define (kaprekars-constant num [iterations 1])
  (if (= num 6174)
      iterations
      (let* ([digit-list (if (< num 1000)
                             (cons 0 (int->digit-list num))
                             (int->digit-list num))]
             [desc (digit-list->int (sort digit-list >))]
             [asc (digit-list->int (sort digit-list <))]
             [diff (- desc asc)])
        ;; (printf "Iteration ~a: ~a\n" iterations diff)
        (kaprekars-constant (- desc asc) (add1 iterations)))))

;; get all valid numbers
(define valid-numbers (filter (lambda (i) (not (apply = (int->digit-list i))))
                              (range 1000 9999)))


;; get the number of iterations for each number
(define iteration-list (map kaprekars-constant valid-numbers))


(plot (points (map vector (range (length iteration-list)) iteration-list)
              #:alpha 0.4
              #:x-jitter 0.5 #:y-jitter 0.1
              #:sym 'fullcircle1 #:color "purple"))

;; get value counts
(define (bagify lst)
  (foldl (lambda (key ht)
           (hash-update ht key add1 0))
         #hash() lst))

(bagify iteration-list)
