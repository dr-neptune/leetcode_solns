#lang racket
(require racket)


#|

idea

Use Stirlings approximation to get the factorial value
Use binary exponentiation to calculate the exponent

then take-while-right while 0

stirlings approximation: n! ~ sqrt(2*pi*n)(n/e)^n

since n >= 0, we can just use expo
|#

(define (expo x n)
  (match n
    [(? zero?) 1]
    [(? even?) (let ([y (expo x (/ n 2))])le
                 (* y y))]
    [_ (* x (expo x (sub1 n)))]))

(define (factorial n)
  (ceiling
   (* (sqrt (* 2 pi n))
      (expo (/ n (exp 1)) n))))

(factorial 8)


(require (only-in math/special-functions gamma)
         (only-in srfi/26 cut)
         (only-in srfi/1 unfold-right))

(gamma (add1 5))

#|

maybe gamma n+1 is best

|#


(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(let ([fct (gamma (add1 50))])
  (length (takef-right (int->digit-list fct) zero?)))
