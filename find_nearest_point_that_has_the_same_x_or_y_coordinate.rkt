#lang racket
(require racket)

(define (shared-manhattan inp comp)
  (cond [(= (first inp) (first comp))
         (list comp (abs (- (second comp) (second inp))))]
        [(= (second inp) (second comp))
         (list comp (abs (- (first comp) (first inp))))]
        [else (list comp -1)]))

(define (last-min p [min-val +inf.0])
  (if (empty? p)
      min-val
      (let ([fdist (cadar p)])
        (if (and (< fdist min-val) (not (= fdist -1)))
            (last-min (rest p) fdist)
            (last-min (rest p) min-val)))))

(define (nearest-valid-point x y points)
  (let* ([pt-distances (map (curry shared-manhattan (list x y)) points)]
         [min-val (last-min pt-distances)]
         [min-distance-location (index-where pt-distances (λ (l) (= (last l) min-val)))])
    (if min-distance-location
        min-distance-location
        -1)))

(define expts '((1 2) (3 1) (2 4) (2 3) (4 4)))
(define exin '(3 4))
(nearest-valid-point 3 4 expts)
