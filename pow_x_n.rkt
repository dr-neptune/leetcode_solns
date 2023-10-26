#lang racket
(require racket srfi/26/cut)

(let ([base 2.1]
      [exp 30])
  (for/fold ([acc 1])
            ([e exp])
            (* base acc)))

(define (my-pow x n)
  (let ([result (for/fold ([acc 1])
                          ([_ (abs n)])
                  (* x acc))])
    (if (negative? n)
        (/ 1 result)
        result)))

(my-pow 2 10)
(my-pow 2 -2)
(my-pow 2 0)


0.00001
2147483647


#|

binary exponentiation

1. convert n to binary
e.g. 3^13 = 3^(1101)
2. split this out into constituents
3^1101 = 3^1 * 3^4 * 3^8
3. for each of the values in (1 4 8) calculate the exponent
do this by squaring at each succession, i.e.
3^1 = 3
(3^1)^2 not in vals
((3^1)^2)^2 = 3^4 in vals

recursive solution:

a^n =
  1 if n == 0
  a^(n/2)^2 if n even and n > 0
  a^((n-1)/2)^2 if n odd and n > 0

|#

(define (expo x n)
  (match n
    [(? zero?) 1]
    [(? even?) (let ([y (expo x (/ n 2))])
                 (* y y))]
    [_ (* x (expo x (/ (sub1 n) 2)))]))

(expo 2 10)


(let loop ([init 2]
           [counter 10])
  (displayln (format "~a ~a" init counter))
  (let ([sq (* init init)])
    (cons sq (loop sq (sub1 counter)))))

(let ([bin (string->list (number->string 13 2))])
  (map (curry * 2) (cons (/ 1 2) (range 1 (length bin)))))

(map (curry * 2) (cons (/ 1 2) (range 1 4)))

(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(accumulate (λ (a b) (* a a)) (identity (range 2 10)))

(define (expo a n)
  (cond
    [(zero? n) 1]
    [(even? n)
     (let [(x (expo a (/ n 2)))]
       (* x x))]
    [else
     (* a (expo a (sub1 n)))]))

(expo 2 0)

(define (my-pow x n)
  (let* ([nn (abs n)]
         [result (cond
                  [(zero? nn) 1]
                  [(even? nn)
                   (let [(x (my-pow x (/ nn 2)))]
                     (* x x))]
                  [else
                   (* x (my-pow x (sub1 nn)))])])
    (if (negative? n)
        (/ 1 result)
        result)))

(my-pow 2 -2)
