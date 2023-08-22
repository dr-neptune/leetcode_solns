#lang racket
(require racket)

;; idea
;; take sum of array
;; integer division by 2 minus 1
;; if n <= ^ then yes, else no

(define (can-place-flowers flowerbed n)
  (let ([spots-taken (foldl + 0 flowerbed)])
    (<= (- (length flowerbed) (* 2 spots-taken)) n)))

;; there are k slots total
;; and l of them are taken
;; each slot must take 2 spaces
;; so we have (- k (* 2 l)) spaces left
;; if there is a flower as an end cap, it takes 2 slots
;; if we are not at an end cap, it will take 3 slots
(can-place-flowers (list 1 0 0 0 1) 1)
(can-place-flowers (list 1 0 0 0 1) 2)
(can-place-flowers (list 1 0 0 0 1 0 0) 2)

;; idea
;; check end caps
;; check inner pots
;; (define (can-place-flowers flowerbed n)
;;   (let* ([front-cap (take flowerbed 2)]
;;          [end-cap (take-right flowerbed 2)]
;;          [rest-beds (drop-right (drop flowerbed 2) 2)])
;;     (match front-cap
;;       [(or (list 1 0) (list 0 1))])))


;; dumb way first
;; iterate through flowerbed
;; if a 1 is encountered, go forward 2
;; else add 1
(define (can-place-flowers flowerbed n)
  (let loop ([flowers flowerbed]
             [open-spots 0])
    (if (null? (rest flowers))
        open-spots
        (match (first flowers)
          [1 (loop (rest flowers) open-spots)]
          [_ (loop (rest flowers) (add1 open-spots))]))))

(can-place-flowers (list 1 0 0 0 1) 1)
(can-place-flowers (list 1 0 0 0 1) 2)
(can-place-flowers exls2 2)

;; (1 0 0 0 1)
;; (1 0) -> no -> (0 0 1)
;; (0 0) -> yes, add1 -> (1)
;; (1) -> no -> return 1
(define (can-place-flowers flowerbed n)
  (let loop ([flowers flowerbed]
             [open-spots 0])
    (if (or (null? flowers)
            (null? (rest flowers)))
        (>= open-spots n)
        (match (take flowers 2)
          [(list 0 0) (loop (rest (rest flowers)) (add1 open-spots))]
          [_ (loop (rest (rest flowers)) open-spots)]))))

;; not done yet

;; (1 0 0 0 1)
;; (1 0 0) -> yes
;; (0 1) -> no

(define (can-place-flowers flowerbed n)
  ())
;; idea
;; split at 1s
;; count up
(define exls '(1 0 0 0 1))

(splitf-at exls (λ (v) (eq? 1 v)))

(define (splitf-at-all ls pred)
  (let loop ([lsv ls]
             [acc '()])
    (cond [(empty? lsv) acc]
          [(pred (first lsv))
           (loop (rest lsv) (cons (first lsv) acc))]
          [else (loop (rest lsv) acc)])))

(splitf-at-all exls (λ (v) (eq? v 1)))

(split-at exls (indexes-of exls 1))

(foldl (λ (a b) (list a (splitf-at b (λ (v) (eq? v 1))))) '() exls)

(map (curry split-at exls) (indexes-of exls 1))

(define (split-into ls indices [acc 0])
  ;; for index in indices
  ;; take values
  ())

(partition (λ (v) (eq? v 1)) '(1 0 0 1 0))

;; some combination argument
;; we have k possible locations
;; and we have n locations taken by flowers already

;; '(1 0 0 0 1)
;; '(1 0 0) -> no
;; '(0 0 0) -> yes
;; '(0 0 1) -> no

;; '(1 0 0 0 1 0 0)
;; '(1 0 0) -> no
;; '(0 0 0) -> yes
;; '(0 0 1) -> no
;; '(0 1 0) -> no
;; '(1 0 0) -> no

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (sliding-window-extended ls n)
  (let ([sw (sliding-window ls n)])
    (cons (take (first sw) 2)
          (cons (rest (last sw))
                sw))))

(define (can-place-flowers flowerbed n)
  (>= (length
       (filter (λ (v) (or (not (member 1 v))
                       (match v
                         [(list 0 0) #t]
                         [_ #f]))) (sliding-window-extended flowerbed 3)))
      n))


(sliding-window-extended '(1 0 0 0 0 1) 3)

(define exls2 '(1 0 0 0 1 0 0))

(sliding-window exls 3)

(filter (λ (v) (or (not (member 1 v))
                   (match v
                     [(list 0 0) #t]
                     [_ #f]))) (sliding-window-extended exls2 3))

(not (member 1 '(1 0 0)))


;; maybe mutate?
;; go through list
;; if prev + next + curr == 0 then add 1 to acc
;; and move ahead 2

;; '(1 0 0 0 0 1)
;; '(1 0 0)
;; '(0 0 0) y acc 1
;; '(0 0 1) n

;; '(1 0 0 0 1)
;; '(1 0 0) n
;; '(0 0 0) y acc 1
;; '(0 1) n

;; '(1 0 0 0 1 0 0)
;; '(1 0 0)
;; '(0 0 0) y acc 1
;; '(0 1 0) n
;; '(1 0 0) n
;; '(0 0) y acc 2

(define (can-place-flowers flowerbed n)
  (cond [(or (zero? (length flowerbed))
             (zero? n)) #f]
        [(eq? (length flowerbed) 1)
         (if (zero? (first flowerbed)) (>= n 1) #f)]
        [else
         (>= (let loop ([sw (reverse (sliding-window-extended flowerbed 3))]
                        [acc 0])
               (if (null? sw)
                   acc
                   (match (first sw)
                     [(list 0 0 0) (loop (rest (rest sw)) (add1 acc))]
                     [(list 0 0) (loop (rest sw) (add1 acc))]
                     [_ (loop (rest sw) acc)]))) n)]))


(let loop ([sw (reverse (sliding-window-extended '(0 0 1 0 0) 3))]
           [acc 0])
        (if (null? sw)
            acc
            (match (first sw)
              [(list 0 0 0) (loop (rest sw) (add1 acc))]
              [(list 0 0) (loop (rest sw) (add1 acc))]
              [_ (loop (rest sw) acc)])))

;; workinonit
