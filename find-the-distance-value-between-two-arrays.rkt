#lang racket
(require racket)

;; naive way
;; for each number in arr1
;; calculate the distance between it and arr2

(define (find-the-distance-value arr1 arr2 d)
  (length
   (filter
    (λ (l) (not (memf (λ (z) (<= z d)) l)))
    (map (λ (y)
           (map (λ (x)
                  (abs (- y x)))
                arr2))
         arr1))))


(find-the-distance-value '(4 5 8) '(10 9 1 8) 2)
(find-the-distance-value '(1 4 2 3) '(-4 -3 6 10 20 30) 3)
(find-the-distance-value '(2 1 100 3) '(-5 -2 10 -3 7) 6)

(find-the-distance-value '(4 5 6 8) '(10 9 1 8) 2)

;; idea
;; sort list 1
;; then see distance at the highest value
;; if d doesn't hold, then return 0
;; if d holds, then take the mid-point and see if it holds for that
;; if so, then take the mid-point again of the LHS, else take the mid-point
;; of the rhs
;; the goal is to triangulate the highest value for which d holds


(define exls '(4 5 8))
(define exls2 '(10 9 1 8))

;; sort list 1
(define exls (sort exls <))

;; check if d holds for the last value
(define (check-d v ls d)
  (if (memf (λ (z) (<= z d)) (map (λ (x) (abs (- v x))) ls)) #t #f))

(check-d 5 exls2 (last exls))

;; it does, so we want to bisect the list

;; try it with lists first, then switch arr1 -> vec

(define (ptr-narrow left right)
  (let* ([pivot (+ left (quotient (- right left) 2))]
         [pivot-val (list-ref arr1 pivot)])
    (cond [()])))

;; essentially we want a binary search pred-max

;; take the halfway point
;; if pred, then try lhs halfway
;; else try rhs halfway
;; when there is 1 element left, if pred, then return the element
;; else return the next element up

(define (find-f-pred predicate ls)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref ls pivot)])
      (begin
        (println (format "left: ~a right: ~a pivot: ~a pivot-val: ~a" left right pivot pivot-val))
        (cond [(>= left right) (if (predicate (list-ref ls left))
                                   (list-ref ls left)
                                   (list-ref ls (add1 right)))]
              [(predicate pivot-val) (ptr-narrow left (sub1 pivot))]
              [else (ptr-narrow (add1 pivot) right)]))))
  (ptr-narrow 0 (sub1 (length ls))))

;; lol, I was recreating findf
;; I wonder if 1 is more efficient than the other
(findf (λ (x) (> x 2)) '(0 1 2 3 4))
(findf (λ (x) (= x 2)) '(0 1 2 3 4))

;; now we want to sort array 1 and then find the first value for which check-d
;; holds. Then everything below it counts
(define exls '(4 5 8))
(define exls2 '(10 9 1 8))

;; sort ls
(define exls (sort exls <))

;; get first val with check-d
(define (check-d v ls d)
  (if (memf (λ (z) (<= z d)) (map (λ (x) (abs (- v x))) ls)) #t #f))

(findf (curryr check-d exls2 2) exls)

(findf (λ (l) (check-d l exls2 2)) exls)


(check-d -8 '(4 10 -4 5 2) 55)

;; trace
[2,1,100,3]
[-5,-2,10,-3,7]
6

(check-d 100 '(-5 -2 10 -3 7) 6)

(define (find-the-distance-value arr1 arr2 d)
  (define (check-d v ls d)
    (if (memf (λ (z) (<= z d))
              (map (λ (x) (abs (- v x))) ls)) #t #f))
  (let ([arr1 (sort arr1 <)])
    (length (filter (λ (x) (> (findf (not (curryr check-d arr2 d)) arr1) x)) arr1))))

(find-the-distance-value '(1 2 3 100) '(-5 -2 10 -3 7) 6)


(findf (λ (l) (not (check-d l '(-5 -2 10 -3 7) 6))) '(1 2 3 100))


;; now we want to get all the values < 8
(length (filter (λ (x) (> (findf (curryr check-d exls2 2) exls) x)) exls))


(define (find-the-distance-value arr1 arr2 d)
  (define (check-d v ls d)
    (if (memf (λ (z) (<= z d))
              (map (λ (x) (abs (- v x))) ls)) #t #f))
  (let ([arr1 (sort arr1 <)])
    (length
     (filter
      (λ (x) (> (findf (not (λ (l) (check-d l arr2 d)))) arr1) x)) arr1)))

(define (check-d v ls d)
  (if (memf (λ (z) (<= z d))
            (map (λ (x) (abs (- v x))) ls)) #t #f))

(define (find-the-distance-value arr1 arr2 d)
  (let ([arr1 (sort arr1 <)])
    (- (length arr1)
       (length
        (filter
         (λ (x) (> (findf (λ (l) (not (check-d l arr2 d))) arr1) x))
         arr1)))))

(find-the-distance-value '(1 2 3 100) '(-5 -2 10 -3 7) 6)


(define (find-the-distance-value arr1 arr2 d)
  (define (check-d v ls d)
    (if (memf (λ (z) (<= z d))
              (map (λ (x) (abs (- v x))) ls)) #t #f))
  (let* ([arr1 (sort arr1 <)]
         [first-pred (findf (curryr check-d arr2 d) arr1)]
         [num-left (length (filter (λ (x) (> first-pred x)) arr1))])
    (if (= 0 num-left)
        1
        num-left)))


;; consider negative values as well

(define (check-d v ls d)
  (if (memf (λ (z) (<= z d))
            (map (λ (x) (abs (- v x))) ls)) #t #f))



(map (λ (x) (abs (- v x))) exls)

(define (find-the-distance-value arr1 arr2 d)
  (let ([arr1 (sort (map abs arr1) <)])
    (filter (λ (x) (> (findf (λ (l) (not (check-d l arr2 d))) arr1) x)) arr1)))


;; try again

;; sort and abs
;; find the first where it doesn't hold
;; then take the length of everything below first val

(define (check-d v ls d)
  (if (memf (λ (z) (<= z d))
            (map (λ (x) (abs (- v x))) ls)) #f #t))


(memf (λ (x) (<= x 6)) (map (λ (x) (abs (- 1 x))) exls2))

(check-d 1 exls2 6)
(check-d 2 exls2 6)
(check-d 3 exls2 6)
(check-d 100 exls2 6)


(findf (λ (x) (check-d x exls2 6)) exls)

(length (filter (λ (x) (>= x (findf (λ (x) (check-d x exls2 6)) exls)))
                exls))

(define (check-d v ls d)
  (if (memf (λ (z) (<= z d))
            (map (λ (x) (abs (- v x))) ls)) #f #t))

(define (find-the-distance-value arr1 arr2 d)
  (length (filter (λ (x) (>= x (findf (λ (x) (check-d x arr2 d)) arr1)))
                  arr1)))


(length
 (filter (λ (x) (>= x (findf (λ (x) (check-d x exls2 2)) exls)))
         exls))

(define exls '(4 5 8))
(define exls2 '(10 9 1 8))

(define exls '(1 2 3 100))
(define exls2 '(-5 -2 10 -3 7))

(findf (λ))

;; implementation of comment
(define (find-the-distance-value arr1 arr2 d)
  (define (is-valid? val)
   (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (list-ref arr2 pivot)])
      (cond [(>= left right) #t]
            [(<= (abs (- pivot-val val)) d) #f]
            [(> pivot-val val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
    (ptr-narrow 0 (sub1 (length arr2))))
  (let ([arr2 (sort arr2 <)])
    (map is-valid? arr1)))

(find-the-distance-value exls exls2 2)
