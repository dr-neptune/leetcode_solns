#lang racket
(require racket)


(define (distance a b) (abs (- a b)))

(define (list-combinations ls1 ls2)
  (for*/list ([v1 (in-list ls1)]
              [v2 (in-list ls2)])
    (list v1 v2)))

(define (flatten-depth lst [depth 1])
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(define (max-distance arrays)
  (for/fold ([acc '()]
             #:result acc ;; (apply max (map (curry apply distance) (flatten-depth acc)))
             )
            ([arr arrays])
    (let* ([min-arr (apply min arr)]
           [max-arr (apply max arr)]
           [rest-maxes
            (filter (λ (v) (not (equal? max-arr v)))
                    (map (curry apply max) arrays))])
      (cons (list-combinations (list min-arr) rest-maxes) acc))))

(define exlol '((1 2 3) (4 5) (1 2 3)))
(define exlol '((1) (1)))
(define exlol '((1 4) (0 5)))
(define exlol '((-1 5 11) (6 10)))

(max-distance exlol)

#|

idea

class Solution {
    public int maxDistance(List<List<Integer>> arrays) {
        int res = 0;
        int n = arrays.get(0).size();
        int min_val = arrays.get(0).get(0);
        int max_val = arrays.get(0).get(arrays.get(0).size() - 1);
        for (int i = 1; i < arrays.size(); i++) {
            n = arrays.get(i).size();
            res = Math.max(res, Math.max(Math.abs(arrays.get(i).get(n - 1) - min_val),
                                         Math.abs(max_val - arrays.get(i).get(0))));
            min_val = Math.min(min_val, arrays.get(i).get(0));
            max_val = Math.max(max_val, arrays.get(i).get(n - 1));
        }
        return res;
    }
}

|#

(define (distance a b) (abs (- a b)))

(let ([arrays exlol])
  (let ([res 0] [n (length (first arrays))] [min-val (first (first arrays))] [max-val (last (first arrays))])
    (for ([idx (in-range 1 (sub1 n))])
      (set! n (list-ref arrays idx))
      (set! res (max res (max (distance (last (list-ref arrays idx)) min-val)
                              (distance max-val (first (list-ref arrays idx))))))
      (set! min-val (min min-val (first (list-ref arrays idx))))
      (set! max-val (max max-val (last (list-ref arrays idx)))))
    res))


(define (max-distance arrays)
  (define (distance a b) (abs (- a b)))
  (let ([res 0] [n (length (first arrays))] [min-val (first (first arrays))] [max-val (last (first arrays))])
    (displayln (format "~a ~a ~a ~a" n res min-val max-val))
    (for ([idx (in-range 1 (sub1 n))])
      (set! n (list-ref arrays idx))
      (set! res (max res (max (distance (last (list-ref arrays idx)) min-val)
                              (distance max-val (first (list-ref arrays idx))))))
      (set! min-val (min min-val (first (list-ref arrays idx))))
      (set! max-val (max max-val (last (list-ref arrays idx))))
      (displayln (format "~a ~a ~a ~a" n res min-val max-val)))
    res))

(max-distance exlol)

(define exlol '((1 2 3) (4 5) (1 2 3)))
(define exlol '((1) (1)))
(define exlol '((1 4) (0 5)))
(define exlol '((-1 5 11) (6 10)))



(define (max-distance arrays)
  (define (distance a b) (abs (- a b)))
  (let ([res 0] [n (length (first arrays))] [min-val (caar arrays)] [max-val (last (first arrays))])
    (for ([idx (in-range n)])
      (set! n (list-ref arrays idx))
      (set! res (max res (max (distance (last (list-ref arrays idx)) min-val)
                              (distance max-val (first (list-ref arrays idx))))))
      (set! min-val (min min-val (first (list-ref arrays idx))))
      (set! max-val (max max-val (last (list-ref arrays idx)))))
    res))
