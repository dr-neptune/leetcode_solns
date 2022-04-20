#lang racket
(require racket)

(define (val-indices ls vals)
  (remove-duplicates
   (flatten
    (for/list ([i vals])
      (indexes-of ls i)))))

(define (get-vals nums target)
  (let ([check (filter (lambda (n)
                         (= (first nums) (- target n)))
                       (rest nums))])
      (if (empty? check)
          (get-vals (rest nums) target)
          (list (first nums)
                (first check)))))


(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (val-indices nums (get-vals nums target)))


(module+ test
  (require rackunit)
  (check-equal? (two-sum '(2 7 11 15) 9) '(0 1))
  (check-equal? (two-sum '(3 2 4) 6) '(1 2))
  (check-equal? (two-sum '(3 3) 6) '(0 1))
  (check-equal? (two-sum '(2 5 5 11) 10) '(1 2)))

;; another go at it
(define exls '(2 7 11 15))

;; idea
;; grab the first item
;; grab the rest of the list
;; map (= target (+ f v)) over the rest of the list
;; if found, return f, v, else iterate

(define (two-sum nums target)
  (let rc ([nums nums]
           [val (first nums)])
    (cond [])))

(first exls)
((λ (f) (member 9 (map (λ (v) (+ f v)) (rest exls)))) 2)

((λ (f) (index-where (rest exls) (λ (v) (= 9 (+ v f))))) (first exls))

(map (λ (f) (index-where (rest exls) (λ (v) (= 9 (+ v f))))) exls)

(define (two-sum nums target)
  (let rc ([ls nums])
    (let* ([fval (first ls)]
           [check ((λ (f)
                     (index-where (rest exls) (λ (v) (= target (+ v f)))))
                   fval)])
      (if check
          (let ([initial-ind (index-of nums fval)])
            (list initial-ind (+ check initial-ind)))
          (rc (rest ls))))))

(two-sum exls 9)
